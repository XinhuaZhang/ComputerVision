{-# LANGUAGE BangPatterns #-}

module Application.GMM.GMM
  ( GMM
  , AssignmentVec
  , getAssignmentVec
  , getAssignmentVecSafe
  , emOne
  , gmmAllSink
  , gmmPartSink
  , readGMM
  , writeGMM
  , initializeGMM
  ) where

import           Application.GMM.Gaussian
import           Application.GMM.MixtureModel
import           Control.DeepSeq              as DS
import           Control.Monad                as M
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           CV.Utility.Parallel
import           CV.Utility.Time
import           Data.Binary
import           Data.ByteString.Lazy         as BL
import           Data.Conduit
import           Data.Conduit.List            as CL
import           Data.List                    as L
import           Data.Maybe
import           Data.Vector                  as V
import           Data.Vector.Unboxed          as VU
import           Prelude                      as P
import           System.Directory
import           System.IO                    as IO
import           Text.Printf

data ResetOption
  = ResetAll
  | ResetIndex !(V.Vector Int)
  deriving (Show)

instance NFData ResetOption where
  rnf (ResetIndex x) = x `seq` ()
  rnf !_             = ()

data EMState a
  = EMDone !Double
           !a
  | EMContinue !AssignmentVec
               !Double
               !Int
               !a
  | EMReset !ResetOption
            !a
  deriving (Show)

instance NFData a =>
         NFData (EMState a) where
  rnf (EMContinue x y n z) = x `seq` y `seq` n `seq` z `seq` ()
  rnf (EMReset x y)      = x `seq` y `seq` ()
  rnf !_                 = ()

type GMM = MixtureModel Gaussian

type AssignmentVec = V.Vector (VU.Vector Double)

initializeGMM :: Int -> ((Double, Double), (Double, Double)) -> IO GMM
initializeGMM numModel' bound = do
  gs <- V.replicateM numModel' (randomGaussian bound)
  initializeMixture numModel' gs

resetGMM :: ResetOption -> GMM -> ((Double, Double), (Double, Double)) -> IO GMM
resetGMM ResetAll gmm bound = initializeGMM (numModel gmm) bound
resetGMM (ResetIndex vec) (MixtureModel n modelVec) bound = do
  gs <- V.replicateM (V.length vec) (randomGaussian bound)
  let !idxModels = V.zip vec gs
  return $!
    MixtureModel
      n
      (V.generate
         n
         (\i ->
             let mi@(Model (wi, _)) = modelVec V.! i
             in case V.find (\(j, _) -> i == j) idxModels of
                  Nothing      -> mi
                  Just (_, gm) -> Model (wi, gm)))

resetGMMList :: ((Double, Double), (Double, Double))
             -> [EMState GMM]
             -> IO [EMState GMM]
resetGMMList bound = P.mapM reset
  where
    reset (EMReset option gmm) = do
      newGMM <- resetGMM option gmm bound
      return $! EMContinue V.empty 1000 0 newGMM
    reset gmmState = return gmmState

getAssignment :: GMM -> Double -> VU.Vector Double
getAssignment (MixtureModel _n modelVec) x = VU.map (/ s) vec
  where
    !vec =
      V.convert .
      V.map
        (\(Model (weight, gaussianModel)) -> weight * gaussian gaussianModel x) $
      modelVec
    !s = VU.sum vec

getAssignmentVec :: GMM -> VU.Vector Double -> AssignmentVec
getAssignmentVec gmm = V.map (getAssignment gmm) . VU.convert

getAssignmentVecSafe :: GMM -> VU.Vector Double -> AssignmentVec
getAssignmentVecSafe gmm =
  V.map
    (\x ->
        let !y = getAssignment gmm x
        in if VU.any isNaN y
             then VU.replicate (VU.length y) 0
             else y) .
  VU.convert

getNks :: AssignmentVec -> VU.Vector Double
getNks = V.foldl1' (VU.zipWith (+))

{-# INLINE getAvgLikelihood #-}

getAvgLikelihood :: GMM -> VU.Vector Double -> Double
getAvgLikelihood gmm xs =
  VU.foldl'
    (\ss x ->
        ss +
        (log .
         V.foldl'
           (\s (Model (weight, gaussianModel)) ->
               s + weight * gaussian gaussianModel x)
           0 .
         model $
         gmm))
    0
    xs /
  fromIntegral (VU.length xs)

updateMu :: AssignmentVec -> VU.Vector Double -> VU.Vector Double -> VU.Vector Double
updateMu assignmentVec nks =
  VU.zipWith (flip (/)) nks .
  V.foldl1' (VU.zipWith (+)) .
  V.zipWith (\assignment x -> VU.map (* x) assignment) assignmentVec . VU.convert

updateSigma
  :: AssignmentVec
  -> VU.Vector Double
  -> VU.Vector Double
  -> VU.Vector Double
  -> VU.Vector Double
updateSigma assignmentVec nks newMu =
  VU.zipWith (flip (/)) nks .
  V.foldl1' (VU.zipWith (+)) .
  V.zipWith
    (\assignment x ->
        VU.zipWith (\a mu -> a * (x - mu) ^ (2 :: Int)) assignment newMu)
    assignmentVec .
  VU.convert

updateW :: Int -> VU.Vector Double -> VU.Vector Double
updateW n w = VU.map (/ VU.sum vec) vec
  where
    !vec = VU.map (/ fromIntegral n) w

updateGMM :: GMM -> AssignmentVec -> VU.Vector Double -> GMM
updateGMM oldGMM oldAssignmentVec xs =
  MixtureModel
    (numModel oldGMM)
    (V.zipWith3
       (\w mu sigma -> Model (w, Gaussian mu sigma))
       (VU.convert newW)
       (VU.convert newMu)
       (VU.convert newSigma))
  where
    !nks = getNks oldAssignmentVec
    !newMu = updateMu oldAssignmentVec nks xs
    !newSigma = updateSigma oldAssignmentVec nks newMu xs
    !newW = updateW (VU.length xs) nks

emOneStep :: Double -> EMState GMM -> VU.Vector Double -> EMState GMM
emOneStep _ x@(EMDone _ _) _ = x
emOneStep threshold (EMContinue oldAssignmentVec oldAvgLikelihood count oldGMM) xs
  | not (V.null zeroNaNNKIdx) = EMReset (ResetIndex zeroNaNNKIdx) oldGMM
  | isJust zeroZIdx = EMReset ResetAll oldGMM
  | rate < threshold || count == 50 = EMDone newAvgLikelihood newGMM
  | otherwise = EMContinue newAssignmentVec newAvgLikelihood (count+1) newGMM
  where
    !nks = getNks oldAssignmentVec
    !zs = V.map VU.sum oldAssignmentVec
    !zeroZIdx = V.findIndex (\x -> x == 0 || isNaN x) zs
    !zeroNaNNKIdx = VU.convert $ VU.findIndices (\x -> x == 0 || isNaN x) nks
    !newGMM = updateGMM oldGMM oldAssignmentVec xs
    !newAssignmentVec = getAssignmentVec newGMM xs
    !newAvgLikelihood = getAvgLikelihood newGMM xs
    !rate = abs $ (oldAvgLikelihood - newAvgLikelihood) / oldAvgLikelihood
emOneStep _ (EMReset _ _) _ =
  error "emOneStep: There models needed to be reset!"

checkStateDone :: EMState a -> Bool
checkStateDone EMDone {} = True
checkStateDone _         = False

checkStateContinueDone :: EMState a -> Bool
checkStateContinueDone EMContinue {} = True
checkStateContinueDone EMDone {}     = True
checkStateContinueDone _             = False

computeStateAssignmentLikelihood :: EMState GMM -> VU.Vector Double -> EMState GMM
computeStateAssignmentLikelihood (EMContinue _ _ c m) x =
  let !assignment = getAssignmentVec m x
      !avgLikelihood = getAvgLikelihood m x
  in EMContinue assignment avgLikelihood c m
computeStateAssignmentLikelihood EMReset {} _ =
  error
    "computeStateAssignment: All reset state shold have been removed by now."
computeStateAssignmentLikelihood state _ = state

getStateLikelihood :: EMState a -> Double
getStateLikelihood (EMContinue _ x _ _) = x
getStateLikelihood (EMDone x _) = x
getStateLikelihood _ =
  error "getStateLikelihood: All reset state shold have been removed by now."

getModelDone :: EMState a -> a
getModelDone (EMDone _ m) = m
getModelDone _ = error "getModelDone: There are states which are not done yet."

getModelContinueDone :: EMState a -> a
getModelContinueDone (EMContinue _ _ _ m) = m
getModelContinueDone (EMDone _ m) = m
getModelContinueDone _ =
  error "getModelContinueDone: There are states which are not EMContinue."

-- Train GMM module for all features once, then writing results to a file and returing results.
emAll
  :: ParallelParams
  -> FilePath
  -> ((Double, Double), (Double, Double))
  -> Double
  -> [EMState GMM]
  -> [VU.Vector Double]
  -> IO [GMM]
emAll parallelParams filePath bound threshold gmms xs =
  if P.all checkStateDone gmms
    then do
      let !avgLikelihood =
            (P.sum . P.map getStateLikelihood $ gmms) /
            fromIntegral (P.length gmms)
          !models = P.map getModelDone gmms
      printCurrentTime
      printf "%0.2f\n" avgLikelihood
      writeGMM filePath models
      return models
    else do
      printCurrentTime
      when
        (P.all checkStateContinueDone gmms)
        (writeGMM filePath (P.map getModelContinueDone gmms))
      gmms1 <- resetGMMList bound gmms
      let !gmms2 =
            parZipWithChunk
              parallelParams
              rdeepseq
              computeStateAssignmentLikelihood
              gmms1
              xs
          !newGMMs =
            parZipWithChunk
              parallelParams
              rdeepseq
              (emOneStep threshold)
              gmms2
              xs
          !avgLikelihood =
            (P.sum . P.map getStateLikelihood $ gmms2) /
            fromIntegral (P.length gmms2)
      if isNaN avgLikelihood
        then IO.putStrLn "Reset"
        else printf "%0.2f\n" avgLikelihood
      emAll parallelParams filePath bound threshold newGMMs xs

-- Train GMM for only one feature, then returing the result.
emOne
  :: Double
  -> GMM
  -> ((Double, Double), (Double, Double))
  -> VU.Vector Double
  -> IO GMM
emOne threshold oldGMM bound xs
  | not (V.null zeroNaNNKIdx) = do
    gmm <- resetGMM (ResetIndex zeroNaNNKIdx) oldGMM bound
    emOne threshold gmm bound xs
  | isJust zeroZIdx = do
    gmm <- resetGMM ResetAll oldGMM bound
    emOne threshold gmm bound xs
  | rate < threshold = return oldGMM
  | otherwise = emOne threshold newGMM bound xs
  where
    !oldAssignmentVec = getAssignmentVec oldGMM xs
    !oldAvgLikelihood = getAvgLikelihood oldGMM xs
    !nks = getNks oldAssignmentVec
    !zs = V.map VU.sum oldAssignmentVec
    !zeroZIdx = V.findIndex (== 0) zs
    !zeroNaNNKIdx = VU.convert $ VU.findIndices (\x -> x == 0 || isNaN x) nks
    !newGMM = updateGMM oldGMM oldAssignmentVec xs
    !newAvgLikelihood = getAvgLikelihood newGMM xs
    !rate = abs $ (oldAvgLikelihood - newAvgLikelihood) / oldAvgLikelihood

-- Train GMM for multiple features, then writing results to a file and returing handle
hEMPart
  :: Handle
  -> ((Double, Double), (Double, Double))
  -> Double
  -> [EMState GMM]
  -> [VU.Vector Double]
  -> IO Handle
hEMPart handle bound threshold gmms xs =
  if P.all checkStateDone gmms
    then do
      let !avgLikelihood =
            (P.sum . P.map getStateLikelihood $ gmms) /
            fromIntegral (P.length gmms)
      printCurrentTime
      printf "%0.2f\n" avgLikelihood
      hPutGMM handle (P.map getModelDone gmms)
      return handle
    else do
      -- printCurrentTime
      gmms1 <- resetGMMList bound gmms
      let !gmms2 = parZipWith rdeepseq computeStateAssignmentLikelihood gmms1 xs
          !newGMMs = parZipWith rdeepseq (emOneStep threshold) gmms2 xs
          !avgLikelihood =
            (P.sum . P.map getStateLikelihood $ gmms2) /
            fromIntegral (P.length gmms2)
      if isNaN avgLikelihood
        then IO.putStrLn "Reset"
        else return ()
             -- do
          -- printf "%0.2f\n" avgLikelihood
          -- print . P.map getStateLikelihood $ gmms2
      hEMPart handle bound threshold newGMMs xs

gmmAllSink
  :: ParallelParams
  -> FilePath
  -> Int
  -> ((Double, Double), (Double, Double))
  -> Double
  -> Int
  -> Sink [VU.Vector Double] (ResourceT IO) [GMM]
gmmAllSink parallelParams filePath numM bound threshold numTrain = do
  xs <- CL.take numTrain
  fileFlag <- liftIO $ doesFileExist filePath
  models <-
    liftIO $
    if fileFlag
      then do
        fileSize <- liftIO $ getFileSize filePath
        if fileSize > 0
          then do
            IO.putStrLn $ "Read GMM data file: " P.++ filePath
            decodeFile filePath
          else M.replicateM (P.length . P.head $ xs) $ initializeGMM numM bound
      else M.replicateM (P.length . P.head $ xs) $ initializeGMM numM bound
  let !ys = P.map VU.concat . L.transpose $ xs
      !stateGMM =
        parZipWithChunk
          parallelParams
          rdeepseq
          (\gmm y ->
              let !assignment = getAssignmentVec gmm y
                  !likelihood = getAvgLikelihood gmm y
              in EMContinue assignment likelihood 0 gmm)
          models
          ys
  liftIO $ emAll parallelParams filePath bound threshold stateGMM ys

gmmPartSink
  :: Handle
  -> [GMM]
  -> ((Double, Double), (Double, Double))
  -> Double
  -> Int
  -> Sink [VU.Vector Double] (ResourceT IO) Handle
gmmPartSink handle gmms bound threshold numTrain = do
  xs <- CL.take numTrain
  liftIO . IO.putStrLn $ "Finish reading data."
  when
    ((P.length . P.head $ xs) /= P.length gmms)
    (error $
     "The number of input features doesn't equal to the number of GMM models. " P.++
     show (P.length . P.head $ xs) P.++
     " vs " P.++
     show (P.length gmms))
  let !ys = P.map VU.concat . L.transpose $ xs
      !stateGMM =
        parZipWith
          rdeepseq
          (\gmm y ->
              let !assignment = getAssignmentVec gmm y
                  !likelihood = getAvgLikelihood gmm y
              in EMContinue assignment likelihood 0 gmm)
          gmms
          ys
  liftIO $ hEMPart handle bound threshold stateGMM ys

hPutGMM :: Handle -> [GMM] -> IO ()
hPutGMM handle =
  M.mapM_
    (\x -> do
       let y = encode x
           len = P.fromIntegral $ BL.length y :: Word32
       BL.hPut handle (encode len)
       BL.hPut handle y)

readGMM :: FilePath -> IO [GMM]
readGMM filePath =
  withBinaryFile
    filePath
    ReadMode
    (\h -> do
       lenbs <- hGet h 4
       let len = fromIntegral (decode lenbs :: Word32) :: Int
       M.replicateM len (hGetGMM h))
  where
    hGetGMM h = do
      sizebs <- BL.hGet h 4
      let size = fromIntegral (decode sizebs :: Word32) :: Int
      bs <- BL.hGet h size
      return $ decode bs

writeGMM :: FilePath -> [GMM] -> IO ()
writeGMM filePath gmms =
  withBinaryFile
    filePath
    WriteMode
    (\h -> do
       BL.hPut h (encode (fromIntegral $ P.length gmms :: Word32))
       hPutGMM h gmms)
