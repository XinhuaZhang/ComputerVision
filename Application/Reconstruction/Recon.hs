{-# LANGUAGE BangPatterns #-}
module Application.Reconstruction.Recon where

import           Control.Monad                  as M
import           CV.Filter.PolarSeparableFilter
import           CV.Utility.FFT
import           CV.Utility.Parallel
import           Data.Array                     as Arr
import           Data.Array.Repa                as R
import           Data.Complex
import qualified Data.Image                     as IM
import           Data.List                      as L
import           Data.Random.Normal
import           Data.Vector                    as V
import           Data.Vector.Storable           as VS
import           Data.Vector.Unboxed            as VU
import           System.Random


data InitRecon
  = InitRecon (VS.Vector (Complex Double))
  | NULL
  deriving (Show)

computeRecon
  :: R.Array D DIM3 (Complex Double)
  -> [VU.Vector (Complex Double)]
  -> R.Array U DIM3 (Complex Double)
computeRecon img filters =
  fromUnboxed (Z :. nf' :. ny' :. nx') .
  VU.concat .
  L.map
    (\imgVec ->
        let coef =
              parMap
                rdeepseq
                (VU.sum . VU.zipWith (*) imgVec . VU.map conjugate)
                filters
        in L.foldl1' (VU.zipWith (+)) $
           parZipWith
             rdeepseq
             (\filterVec c -> VU.map (* c) filterVec)
             filters
             coef) $
  imgList
  where
    (Z :. nf' :. ny' :. nx') = extent img
    imgList =
      L.map
        (\i ->
            toUnboxed . computeUnboxedS . R.slice img $ (Z :. i :. All :. All))
        [0 .. nf']



computeRecon'
  :: R.Array D DIM3 (Complex Double)
  -> [VU.Vector (Complex Double)]
  -> [VU.Vector (Complex Double)]
  -> R.Array U DIM3 (Complex Double)
computeRecon' img filters cFilters =
  fromUnboxed (Z :. nf' :. ny' :. nx') .
  VU.concat .
  L.map
    (\imgVec ->
        let coef = parMap rdeepseq (VU.sum . VU.zipWith (*) imgVec) filters
        in L.foldl1' (VU.zipWith (+)) $
           parZipWith
             rdeepseq
             (\filterVec c -> VU.map (* c) filterVec)
             cFilters
             coef) $
  imgList
  where
    (Z :. nf' :. ny' :. nx') = extent img
    imgList =
      L.map
        (\i ->
            toUnboxed . computeUnboxedS . R.slice img $ (Z :. i :. All :. All))
        [0 .. nf']


magnitudeRecon
  :: Int
  -> Int
  -> Double
  -> Double
  -> VU.Vector (Complex Double)
  -> [VU.Vector (Complex Double)]
  -> Int
  -> IO (VU.Vector (Complex Double))
magnitudeRecon rows cols learningRate threshold img filters writeStep
 = do
  prediction <-
    VU.fromListN (rows * cols) . L.map (:+ 0) <$>
    M.replicateM (rows * cols) (normalIO' (0, 128)) -- (randomRIO valueRange)
  go (normalizeImageU (-1,1) prediction) (read "Infinity" :: Double) (1 :: Int)
  where
    valueRange = (0, 255) :: (Double, Double)
    imgMags = parMap rdeepseq (magnitude . VU.sum . VU.zipWith (*) img) filters
    go !input lastErr timeStep = do
      when
        (mod timeStep writeStep == 0)
        (writeImage (show (div timeStep writeStep) L.++ ".pgm") rows cols (VU.convert input))
      let predictionComplex = parMap rdeepseq (VU.sum . VU.zipWith (*) input) filters
          predictionMags = L.map magnitude predictionComplex
          diff = L.zipWith (\x y -> x ^ (2 :: Int) - y ^ (2 :: Int)) imgMags predictionMags
          err =
            -- L.sum (L.zipWith (\x y -> abs (x - y) / x) imgMags predictionMags) /
            L.sum (L.zipWith (\x y -> abs (x - y)) imgMags predictionMags) / L.sum imgMags /
            (fromIntegral . L.length $ filters) -- L.sum . L.map abs $ diff
          ratio = (lastErr - err) / lastErr
      -- err > lastErr ||
      if -- err > lastErr || 
        err < 0.000 -- ratio < threshold
        then return input
        else do
          let delta =
                L.foldl1' (VU.zipWith (+)) $
                parZipWith3
                  rdeepseq
                  (\d p f ->
                      VU.map (\x -> realPart $ (x) * ((d :+ 0) *  p)) f)
                  diff
                  predictionComplex
                  filters 
          putStrLn $ show err L.++ " " L.++ show ratio
          go
            (VU.zipWith (+) input . VU.map (\x -> x * learningRate :+ 0) $ delta)
            err
            (timeStep + 1)


magnitudeReconConvolution
  :: FFTW
  -> Int
  -> Int
  -> Double
  -> Double
  -> VS.Vector (Complex Double)
  -> [VS.Vector (Complex Double)]
  -> InitRecon
  -> Int -> String
  -> IO (VU.Vector (Complex Double))
magnitudeReconConvolution fftw rows cols learningRate threshold img filters initRecon writeStep prefix = do
  prediction <-
    case initRecon of
      InitRecon x -> return x
      NULL ->
        VS.fromListN (rows * cols) . L.map (:+ 0) <$>
        M.replicateM (rows * cols) (normalIO' (0, 128)) -- (randomRIO valueRange)
  -- IM.writeImage "init.pgm" (IM.arrayToImage .
  --                           listArray ((0, 0), (rows - 1, cols - 1)) . VS.toList . VS.map realPart  $
  --                           prediction :: IM.GrayImage)
  print (VS.minimum . VS.map realPart $ img, VS.maximum . VS.map realPart $ img)
  imgVecF <- dft2d fftw rows cols img
  imgComplex <- M.mapM (idft2d fftw rows cols . VS.zipWith (*) imgVecF) filters
  let imgMags = L.map (VS.map magnitude) imgComplex
  go imgComplex imgMags (normalizeImage (-1,1) prediction) (read "Infinity" :: Double) (1 :: Int)
  where
    valueRange = (0, 255) :: (Double, Double)
    go imgComplex' imgMags' !input lastErr timeStep
                                           -- let input = normalizeImage input' valueRange
     = do
      when
        (mod timeStep writeStep == 0)
        (writeImage (prefix L.++ show (div timeStep writeStep) L.++ ".pgm") rows cols input)
      predictionF <- dft2d fftw rows cols input
      predictionComplex <-
        M.mapM (idft2d fftw rows cols . VS.zipWith (*) predictionF) filters
      let predictionMags = L.map (VS.map magnitude) predictionComplex
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
          dp = parZipWith rdeepseq (VS.zipWith (\d p -> (d :+ 0) * p)) diff predictionComplex
      dpF <- M.mapM (dft2d fftw rows cols) dp
      if err < threshold -- || err > lastErr
        then return . VS.convert $ input :: IO (VU.Vector (Complex Double))
        else do
          delta <-
            L.foldl1' (VS.zipWith (+)) <$>
            M.zipWithM
              (\dp' f' ->
                  fmap (VS.map realPart) . idft2d fftw rows cols $
                  VS.zipWith (*) dp' f')
              dpF
              filters
          putStrLn $ show err L.++ " " L.++ show errComplex
          go
            imgComplex'
            imgMags'
            (VS.zipWith (+) input . VS.map (\x -> x * learningRate :+ 0) $! delta)
            err
            (timeStep + 1)


-- L.foldl1' (VS.zipWith (+)) $
-- parZipWith3
--   rdeepseq
--   (\d p (af, rf) ->
--       VS.generate
--         (rows * cols)
--         (\i ->
--             VS.sum $
--             VS.izipWith
--               (\j dj pj ->
--                   let (xi, yi) = idx VU.! i
--                       (xj, yj) = idx VU.! j
--                   in realPart $!
--                      (dj :+ 0) *
--                      fourierMellinTransform
--                        0
--                        rf
--                        af
--                        (xj - xi)
--                        (yj - yi) *
--                      conjugate pj)
--               d
--               p))
--   diff
--   predictionComplex
--   freqs

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
    
    

{-# INLINE normalizeImageU #-}

normalizeImageU :: (Double, Double)
               -> VU.Vector (Complex Double)
               -> VU.Vector (Complex Double)
normalizeImageU (minVal, maxVal) vec =
  VU.map
    (\x ->
        ((x - imgMinVal) / (imgMaxVal - imgMinVal) * (maxVal - minVal) + minVal) :+
        0)
    vec'
  where
    vec' = VU.map realPart vec
    imgMinVal = VU.minimum vec'
    imgMaxVal = VU.maximum vec'

{-# INLINE removeMean #-}

removeMean :: VU.Vector Double -> VU.Vector Double
removeMean vec = VU.map (\x -> x - m) vec
  where
    m = VU.sum vec / fromIntegral (VU.length vec)

{-# INLINE imageMean #-}

imageMean :: VU.Vector Double -> Double
imageMean vec = VU.sum vec / fromIntegral (VU.length vec)


{-# INLINE writeImage #-}

writeImage :: FilePath -> Int -> Int -> VS.Vector (Complex Double) -> IO ()
writeImage filePath rows cols =
  IM.writeImage filePath .
  IM.realPart .
  IM.arrayToImage .
  listArray ((0, 0), (rows - 1, cols - 1)) .
  VS.toList . normalizeImage (0, 255) . VU.convert
