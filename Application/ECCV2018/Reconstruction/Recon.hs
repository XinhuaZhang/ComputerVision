module Application.ECCV2018.Reconstruction.Recon where

import           Control.Monad          as M
import           Control.Monad.Parallel as MP
import           CV.Utility.DFT
import           CV.Utility.Utility
import           CV.Utility.Parallel
import           Data.Array             as Arr
import           Data.Array.Repa        as R
import           Data.Complex
import qualified Data.Image             as IM
import           Data.List              as L
import           Data.Random.Normal
import           Data.Vector            as V
import           Data.Vector.Storable   as VS
import           Data.Vector.Unboxed    as VU
import           System.Directory
import           System.FilePath
import           System.Random
import           Text.Printf

data InitRecon
  = InitRecon (VS.Vector (Complex Double))
  | NULL
  deriving (Show)

{-# INLINE writeImage #-}

writeImage :: FilePath -> Int -> Int -> VS.Vector (Complex Double) -> IO ()
writeImage filePath rows cols =
  IM.writeImage filePath .
  IM.realPart .
  IM.arrayToImage .
  listArray ((0, 0), (rows - 1, cols - 1)) .
  VS.toList . normalizeImage (0, 255) . VU.convert

{-# INLINE normalizeImage #-}

normalizeImage :: (Double, Double)
               -> VS.Vector (Complex Double)
               -> VS.Vector (Complex Double)
normalizeImage (minVal, maxVal) vec =
  VS.map
    (\x ->
        ((x - imgMinVal) / (imgMaxVal - imgMinVal) * (maxVal - minVal) + minVal) :+
        0)
    vec'
  where
    vec' = VS.map realPart vec
    imgMinVal = VS.minimum vec'
    imgMaxVal = VS.maximum vec'
    
{-# INLINE normalizeMagnitudes #-}

normalizeMagnitudes :: [VS.Vector Double] -> [VS.Vector Double]
normalizeMagnitudes =
  L.map VS.fromList .
  L.transpose .
  parMap rdeepseq (VU.toList . l2norm . VU.fromList) .
  L.transpose . L.map VS.toList  
  
{-# INLINE normalizeComplex #-}

normalizeComplex :: [VS.Vector (Complex Double)] -> [VS.Vector (Complex Double)]
normalizeComplex =
  L.map VS.fromList .
  L.transpose .
  parMap
    rdeepseq
    (\xs ->
       let ys = VU.fromList xs
           l2 = sqrt . VU.sum . VU.map (^ (2 :: Int)) . VU.map magnitude $ ys
       in if l2 == 0
            then xs
            else VU.toList . VU.map (/ (l2 :+ 0)) $ ys) .
  L.transpose . L.map VS.toList  



magnitudeReconConvolution
  :: DFTPlan
  -> Int
  -> Int
  -> Double
  -> Double
  -> VS.Vector (Complex Double)
  -> [VS.Vector (Complex Double)]
  -> [VS.Vector (Complex Double)]
  -> InitRecon
  -> Int
  -> String
  -> IO (VU.Vector (Complex Double))
magnitudeReconConvolution plan rows cols learningRate threshold img filters filtersPI initRecon writeStep folderName = do
  removePathForcibly folderName
  createDirectoryIfMissing True folderName
  prediction <-
    case initRecon of
      InitRecon x -> return x
      NULL ->
        VS.fromList . L.map (:+ 0) <$>
        M.replicateM (rows * cols) (normalIO' (0, 128))
  imgVecF <- dftExecute plan (DFTPlanID DFT2D [rows, cols] []) img
  imgComplex <-
    fmap normalizeComplex .
    MP.mapM
      (dftExecute plan (DFTPlanID IDFT2D [rows, cols] []) .
       VS.zipWith (*) imgVecF) $
    filters
  let imgMags = L.map (VS.map magnitude) $ imgComplex
  go
    imgComplex
    imgMags
    (normalizeImage (-1, 1) prediction)
    (read "Infinity" :: Double)
    (1 :: Int)
  where
    dftPlanID = DFTPlanID DFT2D [rows, cols] []
    dftPlan = getDFTPlan plan dftPlanID
    idftPlanID = DFTPlanID IDFT2D [rows, cols] []
    idftPlan = getDFTPlan plan idftPlanID
    go imgComplex' imgMags' input lastErr timeStep = do
      when
        (mod timeStep writeStep == 0)
        (writeImage
           (folderName </> (printf "%05d" (div timeStep writeStep)) L.++ ".pgm")
           rows
           cols
           input)
      predictionF <- dftExecute plan dftPlanID input
      predictionComplex <-
        fmap normalizeComplex .
        MP.mapM
          (dftExecuteWithPlan idftPlanID idftPlan . VS.zipWith (*) predictionF) $
        filters
      let predictionMags
            -- normalizeMagnitudes .
           = L.map (VS.map magnitude) $ predictionComplex
          diff =
            parZipWith
              rdeepseq
              (VS.zipWith (\x y -> x ^ (2 :: Int) - y ^ (2 :: Int)))
              imgMags'
              predictionMags
          err =
            L.sum
              (L.zipWith
                 (\x y -> (VS.sum . VS.map abs $ VS.zipWith (-) x y) / VS.sum x)
                 imgMags'
                 predictionMags) /
            (fromIntegral . L.length $ filters)
          errComplex =
            L.sum
              (L.zipWith
                 (\x y ->
                    (VS.sum . VS.map magnitude $ VS.zipWith (-) x y) /
                    (VS.sum . VS.map magnitude $ x))
                 imgComplex'
                 predictionComplex) /
            (fromIntegral . L.length $ filters)
          dp =
            parZipWith
              rdeepseq
              (VS.zipWith (\d p -> (d :+ 0) * conjugate p))
              diff
              predictionComplex
      dpF <- MP.mapM (dftExecuteWithPlan dftPlanID dftPlan) dp
      if err < threshold -- || err > lastErr
        then return . VS.convert $ input :: IO (VU.Vector (Complex Double))
        else do
          delta <-
            fmap (L.foldl1' (VS.zipWith (+))) . MP.sequence $
            L.zipWith
              (\dp' f' ->
                 fmap (VS.map realPart) .
                 (dftExecuteWithPlan idftPlanID idftPlan) $
                 VS.zipWith (*) dp' f')
              dpF
              filtersPI
          putStrLn $ show err L.++ " " L.++ show errComplex
          go
            imgComplex'
            imgMags'
            (VS.zipWith (+) input . VS.map (\x -> x * learningRate :+ 0) $!
             delta)
            err
            (timeStep + 1)
