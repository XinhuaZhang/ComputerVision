{-# LANGUAGE BangPatterns #-}

module Application.MultiDimensionalGMM.GMM where

import           Application.MultiDimensionalGMM.Gaussian
import           Application.MultiDimensionalGMM.MixtureModel
import           Control.DeepSeq                              as DS
import           Control.Monad                                as M
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           CV.Feature.PolarSeparable
import           CV.Utility.Parallel
import           CV.Utility.Time
import           Data.Array.Repa                              as R
import           Data.Binary
import qualified Data.ByteString.Lazy                         as BL
import           Data.Conduit
import           Data.Conduit.List                            as CL
import           Data.List                                    as L
import           Data.Maybe
import           Data.Vector                                  as V
import           Data.Vector.Unboxed                          as VU
import           Prelude                                      as P
import           System.Directory
import           System.IO                                    as IO
import           Text.Printf

type GMM = MixtureModel Gaussian

type AssignmentVec = [[Double]]

data ResetOption
  = ResetAll
  | ResetIndex ![Int]
  deriving (Show)

instance NFData ResetOption where
  rnf !_ = ()

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
  rnf !_ = ()

initializeGMM :: Int -> Int -> ((Double, Double), (Double, Double)) -> IO GMM
initializeGMM numModel' numDim bound = do
  gs <- M.replicateM numModel' (randomGaussian numDim bound)
  initializeMixture numModel' gs

resetGMM :: ResetOption -> GMM -> ((Double, Double), (Double, Double)) -> IO GMM
resetGMM ResetAll gmm bound =
  initializeGMM
    (numModel gmm)
    (VU.length . gaussianMu . snd . (\(Model x') -> x') . L.head . model $ gmm)
    bound
resetGMM (ResetIndex idx) (MixtureModel n models) bound = do
  gs <- V.replicateM (V.length vec) (randomGaussian nd bound)
  let !idxModels = V.zip vec gs
  return $!
    MixtureModel
      n
      (V.toList $
       V.generate
         n
         (\i ->
             let mi@(Model (wi, _)) = modelVec V.! i
             in case V.find (\(j, _) -> i == j) idxModels of
                  Nothing      -> mi
                  Just (_, gm) -> Model (wi, gm)))
  where
    !vec = V.fromList idx
    !modelVec = V.fromListN n models
    !nd = VU.length . gaussianMu . snd . (\(Model x') -> x') . L.head $ models

getAssignment :: GMM -> VU.Vector Double -> [Double]
getAssignment (MixtureModel _n models) x'
  | s == 0 = ys
  | otherwise = L.map (/ s) ys
  where
    !ys =
      L.map
        (\(Model (weight, gaussianModel)) -> weight * gaussian gaussianModel x')
        models
    !s = L.sum ys

getAssignmentVec :: GMM -> [VU.Vector Double] -> AssignmentVec
getAssignmentVec gmm = L.map (getAssignment gmm)

getNks :: AssignmentVec -> [Double]
getNks = L.foldl1' (L.zipWith (+))

updateMu :: AssignmentVec
         -> [Double]
         -> [VU.Vector Double]
         -> [VU.Vector Double]
updateMu assignmentVec nks =
  L.zipWith (\nk x' -> VU.map (/ nk) x') nks .
  L.foldl1' (L.zipWith (VU.zipWith (+))) .
  L.zipWith (\assignment x' -> L.map (\y' -> VU.map (* y') x') assignment) assignmentVec

updateSigma
  :: AssignmentVec
  -> [Double]
  -> [VU.Vector Double]
  -> [VU.Vector Double]
  -> [VU.Vector Double]
updateSigma assignmentVec nks newMu =
  L.zipWith (\nk x' -> VU.map (/ nk) x') nks .
  L.foldl1' (L.zipWith (VU.zipWith (+))) .
  L.zipWith
    (\assignment x' ->
        L.zipWith
          (\a mu -> VU.map (* a) $ VU.zipWith (\y' m -> (y' - m) ^ (2 :: Int)) x' mu)
          assignment
          newMu)
    assignmentVec

updateW :: Int -> [Double] -> [Double]
updateW n w = L.map (/ L.sum vec) vec
  where
    !vec = L.map (/ fromIntegral n) w

updateGMM :: GMM -> AssignmentVec -> [VU.Vector Double] -> GMM
updateGMM oldGMM oldAssignmentVec xs =
  MixtureModel
    (numModel oldGMM)
    (L.zipWith3 (\w m s -> Model (w, Gaussian m s)) newW newMu newSigma)
  where
    !nks = getNks oldAssignmentVec
    !newMu = updateMu oldAssignmentVec nks xs
    !newSigma = updateSigma oldAssignmentVec nks newMu xs
    !newW = updateW (L.length xs) nks

getAvgLikelihood :: GMM -> [VU.Vector Double] -> Double
getAvgLikelihood gmm xs =
  L.foldl'
    (\ss x' ->
        ss +
        (log .
         L.foldl'
           (\s (Model (weight, gaussianModel)) ->
               s + weight * gaussian gaussianModel x')
           0 .
         model $
         gmm))
    0
    xs /
  fromIntegral (L.length xs)

em
  :: Double
  -> Int
  -> GMM
  -> ((Double, Double), (Double, Double))
  -> [VU.Vector Double]
  -> IO GMM
em threshold count' oldGMM bound xs
  | not (L.null zeroNaNNKIdx) = do
    print zeroNaNNKIdx
    gmm <- resetGMM (ResetIndex zeroNaNNKIdx) oldGMM bound
    em threshold count' gmm bound xs
  | isJust zeroZIdx = do
    putStrLn "Reset all"
    gmm <- resetGMM ResetAll oldGMM bound
    em threshold count' gmm bound xs
  | rate < threshold || count' == 50 = do
    printCurrentTime
    printf "%0.2f\n" oldAvgLikelihood
    return oldGMM
  | otherwise = do
    printCurrentTime
    printf "%0.2f\n" oldAvgLikelihood
    em threshold (count' + 1) newGMM bound xs
  where
    !oldAssignmentVec = getAssignmentVec oldGMM xs
    !oldAvgLikelihood = getAvgLikelihood oldGMM xs
    !nks = getNks oldAssignmentVec
    !zs = L.map L.sum oldAssignmentVec
    !zeroZIdx = L.elemIndex 0 zs
    !zeroNaNNKIdx = L.findIndices (\x' -> x' == 0 || isNaN x') nks
    !newGMM = updateGMM oldGMM oldAssignmentVec xs
    !newAvgLikelihood = getAvgLikelihood newGMM xs
    !rate = abs $ (oldAvgLikelihood - newAvgLikelihood) / oldAvgLikelihood

gmmSink
  :: FilePath
  -> Int
  -> ((Double, Double), (Double, Double))
  -> Double
  -> Int
  -> Sink [VU.Vector Double] (ResourceT IO) ()
gmmSink filePath numM bound threshold numTrain = do
  xs <- CL.take numTrain
  fileFlag <- liftIO $ doesFileExist filePath
  let !nd = VU.length . L.head . L.head $ xs
      !ys = L.concat xs
  gmm <-
    liftIO $
    if fileFlag
      then do
        fileSize <- liftIO $ getFileSize filePath
        if fileSize > 0
          then do
            IO.putStrLn $ "Read GMM data file: " P.++ filePath
            decodeFile filePath
          else initializeGMM numM nd bound
      else initializeGMM numM nd bound
  newGMM <- liftIO $ em threshold 0 gmm bound ys
  liftIO $ encodeFile filePath newGMM

hGMMSink
  :: ParallelParams
  -> Handle
  -> Int
  -> ((Double, Double), (Double, Double))
  -> Double
  -> Int
  -> Sink (Array U DIM3 Double) (ResourceT IO) [Array U DIM3 Double]
hGMMSink parallelParams handle numM bound threshold numTrain = do
  arrs <- CL.take numTrain
  let !xs = parMapChunk parallelParams rdeepseq extractPointwiseFeature arrs
      !nd = VU.length . L.head . L.head $ xs
      !ys = L.concat xs
  gmm <- liftIO $ initializeGMM numM nd bound
  newGMM <- liftIO $ em threshold 0 gmm bound ys
  liftIO $ hPutGMM handle newGMM
  return arrs

hPutGMM :: Handle -> GMM -> IO ()
hPutGMM handle gmm = do
  BL.hPut handle (encode len)
  BL.hPut handle encodedGMM
  where
    !encodedGMM = encode gmm
    !len = fromIntegral $ BL.length encodedGMM :: Word32

readGMM :: FilePath -> IO [GMM]
readGMM filePath = withBinaryFile filePath ReadMode hGetGMM
  where
    hGetGMM h = do
      sizebs <- BL.hGet h 4
      if BL.length sizebs < 4
        then return []
        else do
          let !size' = fromIntegral (decode sizebs :: Word32) :: Int
          bs <- BL.hGet h size'
          gmms <- hGetGMM h
          return $! decode bs : gmms

writeGMM :: FilePath -> [GMM] -> IO ()
writeGMM filePath gmms =
  withBinaryFile filePath WriteMode (\h -> M.mapM_ (hPutGMM h) gmms)
