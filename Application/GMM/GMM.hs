{-# LANGUAGE BangPatterns #-}

module Application.GMM.GMM
  ( GMM
  , AssignmentVec
  , getAssignmentVec
  , gmmSink
  ) where

import           Application.GMM.Gaussian
import           Application.GMM.MixtureModel
import           Control.DeepSeq              as DS
import           Control.Monad                as M
import           Control.Monad.IO.Class
import           CV.Utility.Parallel
import           CV.Utility.Time
import           Data.Binary
import           Data.Conduit
import           Data.Conduit.List            as CL
import           Data.List                    as L
import           Data.Maybe
import           Data.Vector                  as V
import           Data.Vector.Unboxed          as VU
import           Prelude                      as P
import           System.Directory
import           Text.Printf
import Data.ByteString.Lazy as BL
import System.IO as IO 

data ResetOption
  = ResetAll
  | ResetIndex !(V.Vector Int)
  deriving (Show)

instance NFData ResetOption where
  rnf (ResetIndex x) = x `seq` ()
  rnf _              = ()

data EMState a
  = EMDone !Double
           !a
  | EMContinue AssignmentVec
               Double
               !a
  | EMReset !ResetOption
            !a

instance NFData a =>
         NFData (EMState a) where
  rnf (EMContinue x y z) = x `seq` y `seq` z `seq` ()
  rnf (EMReset x _y)     = x `seq` ()
  rnf _                  = ()

type GMM = MixtureModel Gaussian

type AssignmentVec = V.Vector (VU.Vector Double)

initializeGMM :: Int -> ((Double, Double),(Double, Double)) -> IO GMM
initializeGMM numModel' bound = do
  gs <- V.replicateM numModel' (randomGaussian bound)
  initializeMixture numModel' gs

resetGMM :: ResetOption -> GMM -> ((Double, Double),(Double, Double)) -> IO GMM
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

resetGMMList :: ((Double, Double),(Double, Double)) -> [EMState GMM] -> IO [EMState GMM]
resetGMMList bound = P.mapM reset
  where
    reset (EMReset option gmm) = do
      newGMM <- resetGMM option gmm bound
      return $! EMContinue undefined undefined newGMM
    reset gmmState = return gmmState

{-# INLINE getAssignment #-}

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

getNks :: AssignmentVec -> VU.Vector Double
getNks = V.foldl1' (VU.zipWith (+))

getAvgLikelihood :: GMM -> VU.Vector Double -> Double
getAvgLikelihood gmm xs =
  VU.foldl' (\ss x ->
               ss +
               (log .
                V.foldl' (\s (Model (weight,gaussianModel)) ->
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
updateW n = VU.map (/ fromIntegral n)

emOneStep :: Double -> EMState GMM -> VU.Vector Double -> EMState GMM
emOneStep _ x@(EMDone _ _) _ = x
emOneStep threshold (EMContinue oldAssignmentVec _ oldGMM) xs
  | not (V.null zeroNaNNKIdx) = EMReset (ResetIndex zeroNaNNKIdx) oldGMM
  | isJust zeroZIdx = EMReset ResetAll oldGMM
  | newAvgLikelihood > threshold = EMDone newAvgLikelihood newGMM
  | otherwise = EMContinue newAssignmentVec newAvgLikelihood newGMM
  where !nks = getNks oldAssignmentVec
        !newMu = updateMu oldAssignmentVec nks xs
        !newSigma = updateSigma oldAssignmentVec nks newMu xs
        !newW = updateW (VU.length xs) nks
        !zs = V.map VU.sum oldAssignmentVec
        !zeroZIdx = V.findIndex (== 0) zs
        !zeroNaNNKIdx =
          VU.convert $
          VU.findIndices (\x -> x == 0 || isNaN x)
                         nks
        !newGMM =
          MixtureModel
            (numModel oldGMM)
            (V.zipWith3 (\w mu sigma -> Model (w,Gaussian mu sigma))
                        (VU.convert newW)
                        (VU.convert newMu)
                        (VU.convert newSigma))
        !newAssignmentVec = getAssignmentVec newGMM xs
        !newAvgLikelihood = getAvgLikelihood newGMM xs
emOneStep _ (EMReset _ _) _ =
  error "emOneStep: There models needed to be reset!"

em
  :: ParallelParams
  -> FilePath
  -> ((Double, Double),(Double, Double))
  -> Double
  -> [EMState GMM]
  -> [VU.Vector Double]
  -> IO ()
em parallelParams filePath bound threshold gmms xs =
  if P.all checkStateDone gmms
     then do let !avgLikelihood =
                   (P.sum . P.map getStateLikelihood $ gmms) /
                   fromIntegral (P.length gmms)
             printCurrentTime
             printf "%0.2f\n" avgLikelihood
             encodeFile filePath
                        (P.map getModelDone gmms)
     else do printCurrentTime
             when (P.all checkStateContinueDone gmms)
                  (encodeFile filePath
                              (P.map getModelContinueDone gmms))
             gmms1 <- resetGMMList bound gmms
             let !gmms2 =
                   parZipWithChunk parallelParams rdeepseq computeStateAssignmentLikelihood gmms1 xs
                 !newGMMs =
                   parZipWithChunk parallelParams
                                   rdeepseq
                                   (emOneStep threshold)
                                   gmms2
                                   xs
                 !avgLikelihood =
                   (P.sum . P.map getStateLikelihood $ gmms) /
                   fromIntegral (P.length gmms)
             if isNaN avgLikelihood
                then IO.putStrLn "Reset"
                else printf "%0.2f\n" avgLikelihood
             em parallelParams filePath bound threshold newGMMs xs
  where checkStateDone EMDone{} = True
        checkStateDone _        = False
        checkStateContinueDone EMContinue{} = True
        checkStateContinueDone EMDone{}     = True
        checkStateContinueDone _            = False
        computeStateAssignmentLikelihood (EMContinue _ _ m) x =
          let !assignment = getAssignmentVec m x
              !avgLikelihood = getAvgLikelihood m x
          in EMContinue assignment avgLikelihood m
        computeStateAssignmentLikelihood EMReset{} _ =
          error "computeStateAssignment: All reset state shold have been removed by now."
        computeStateAssignmentLikelihood state _ = state
        getStateLikelihood (EMContinue _ x _) = x
        getStateLikelihood (EMDone x _) = x
        getStateLikelihood _ =
          error "getStateLikelihood: All reset state shold have been removed by now."
        getModelDone (EMDone _ m) = m
        getModelDone _ =
          error "getModelDone: There are states which are not done yet."
        getModelContinueDone (EMContinue _ _ m) = m
        getModelContinueDone (EMDone _ m) = m
        getModelContinueDone _ =
          error "getModelContinueDone: There are states which are not EMContinue."
          



gmmSink
  :: ParallelParams
  -> FilePath
  -> Int
  -> ((Double, Double),(Double, Double))
  -> Double
  -> Sink [VU.Vector Double] IO ()
gmmSink parallelParams filePath numM bound threshold =
  do xs <- consume
     fileFlag <- liftIO $ doesFileExist filePath
     models <-
       liftIO $
       if fileFlag
          then do fileSize <- liftIO $ getFileSize filePath
                  if fileSize > 0
                     then do IO.putStrLn $ "Read GMM data file: " P.++ filePath
                             decodeFile filePath
                     else M.replicateM (P.length . P.head $ xs)  $
                          initializeGMM numM
                                        bound
          else M.replicateM (P.length . P.head $ xs) $
               initializeGMM numM
                             bound
     let !ys = P.map VU.concat . L.transpose $ xs
         !stateGMM =
           parZipWithChunk
             parallelParams
             rdeepseq
             (\gmm y ->
                let !assignment = getAssignmentVec gmm y
                    !likelihood = getAvgLikelihood gmm y
                in EMContinue assignment likelihood gmm)
             models
             ys
     liftIO $ em parallelParams filePath bound threshold stateGMM ys


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
