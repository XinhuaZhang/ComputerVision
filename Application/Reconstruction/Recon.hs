{-# LANGUAGE BangPatterns #-}
module Application.Reconstruction.Recon where

import           Control.Monad                  as M
import           CV.Filter.GaussianFilter
import           CV.Filter.PinwheelWavelet
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
                      VU.map (\x -> realPart $ (x) * ((d :+ 0) *  conjugate p)) f)
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
  -> [VS.Vector (Complex Double)] -> [VS.Vector (Complex Double)]
  -> InitRecon
  -> Int
  -> String
  -> IO (VU.Vector (Complex Double))
magnitudeReconConvolution fftw rows cols learningRate threshold img filters filtersPI initRecon writeStep prefix = do
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
  go
    imgComplex
    imgMags
    (normalizeImage (-1, 1) prediction)
    (read "Infinity" :: Double)
    (1 :: Int)
  where
    valueRange = (0, 255) :: (Double, Double)
    go imgComplex' imgMags' !input lastErr timeStep
                                           -- let input = normalizeImage input' valueRange
     = do
      when
        (mod timeStep writeStep == 0)
        (writeImage
           (prefix L.++ show (div timeStep writeStep) L.++ ".pgm")
           rows
           cols
           input)
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
          dp =
            parZipWith
              rdeepseq
              (VS.zipWith (\d p -> (d :+ 0) * conjugate p))
              diff
              predictionComplex
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
              filtersPI
          putStrLn $ show err L.++ " " L.++ show errComplex
          go
            imgComplex'
            imgMags'
            (VS.zipWith (+) input . VS.map (\x -> x * learningRate :+ 0) $! delta)
            err
            (timeStep + 1)



magnitudeSegmentation
  :: FFTW
  -> Int
  -> Int
  -> Double
  -> Double
  -> Double
  -> VS.Vector (Complex Double)
  -> [VS.Vector (Complex Double)]
  -> [VS.Vector (Complex Double)]
  -> GaussianFilterConvolution
  -> InitRecon
  -> Int
  -> String
  -> IO (VU.Vector (Complex Double))
magnitudeSegmentation fftw rows cols lambda learningRate threshold img filters filtersPI gFilters initRecon writeStep prefix = do
  prediction <-
    case initRecon of
      InitRecon x -> return x
      NULL ->
        VS.fromListN (rows * cols) . L.map (:+ 0) <$>
        M.replicateM (rows * cols) (normalIO' (-1, 1 :: Double))
  imgVecF <- dft2d fftw rows cols (normalizeImage (-1, 1) img)
  imgComplex <- M.mapM (idft2d fftw rows cols . VS.zipWith (*) imgVecF) filters
  let imgMags = L.map (VS.map magnitude) imgComplex
  writeImage ("image.pgm") rows cols .
    VS.map
      (\(a :+ 0) ->
          if a > 0
            then 1 :+ 0
            else (-1) :+ 0) .
    normalizeImage (-1, 1) $
    img
  go
    imgComplex
    imgMags
    -- normalizeImage (-1, 1)
    (prediction)
    (read "Infinity" :: Double)
    (1 :: Int)
  where
    go imgComplex' imgMags' !input lastErr timeStep = do
      when
        (mod timeStep writeStep == 0)
        (writeImage
           (prefix L.++ show (div timeStep writeStep) L.++ ".pgm")
           rows
           cols .
         VS.map
           (\(a :+ 0) ->
               if a > 0
                 then 1 :+ 0
                 else (-1) :+ 0) $
         input)
      predictionF <- dft2d fftw rows cols input
      predictionComplex <-
        M.mapM (idft2d fftw rows cols . VS.zipWith (*) predictionF) filters
      predictionGaussian <- applyFilterConvolution fftw gFilters [ input]
      let predictionMags = L.map (VS.map magnitude) predictionComplex
          diff =
            parZipWith
              rdeepseq
              (VS.zipWith (\x y -> x ^ (2 :: Int) - y ^ (2 :: Int)))
              imgMags'
              predictionMags
          err =
            (L.sum
               (L.zipWith
                  (\x y -> (VS.sum . VS.map abs $ VS.zipWith (-) x y) / VS.sum x)
                  imgMags'
                  predictionMags)) /
            (fromIntegral . L.length $ filters) +
            lambda *
            (VS.sum . VS.map (\a -> (realPart a) ** (-2)) . L.head $ predictionGaussian)
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
          predictionGaussian1 = L.map (VS.map (\y -> y ** (-3))) predictionGaussian
      dpF <- M.mapM (dft2d fftw rows cols) dp
      predictionGaussian2 <- applyFilterConvolution fftw gFilters predictionGaussian1
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
              filtersPI
          putStrLn $ show err L.++ " " L.++ show errComplex
          go
            imgComplex'
            imgMags'
            (normalizeImage (-1, 1) .
             VS.zipWith (+) input .
             VS.map (\x -> x * learningRate :+ 0) .
             VS.zipWith (\a b -> a - lambda * realPart b) delta . L.head $
             predictionGaussian2)
            err
            (timeStep + 1)

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




pinwheelWaveletMagnitudeRecon
  :: Int
  -> Int
  -> Double
  -> Double
  -> VU.Vector (Complex Double)
  -> PinwheelWaveletExpansion
  -> PinwheelWaveletPIExpansion
  -> Int
  -> IO (VU.Vector (Complex Double))
pinwheelWaveletMagnitudeRecon rows cols learningRate threshold img filters (PinwheelWaveletPIExpansion inverseFilters) writeStep = do
  prediction <-
    VU.fromListN (rows * cols) . L.map (:+ 0) <$>
    M.replicateM (rows * cols) (normalIO' (0, 128))
  go (normalizeImageU (-1, 1) prediction) (read "Infinity" :: Double) (1 :: Int)
  where
    imgMags =
      L.concatMap (L.map (L.map magnitude)) $
      applyPinwheelWaveletExpansion filters [img]
    go !input lastErr timeStep = do
      when
        (mod timeStep writeStep == 0)
        (writeImage
           (show (div timeStep writeStep) L.++ ".pgm")
           rows
           cols
           (VU.convert input))
      let predictionComplex =
            L.concat $ applyPinwheelWaveletExpansion filters [input]
          predictionMags = L.map (L.map magnitude) predictionComplex
          err =
            L.sum
              (L.zipWith
                 (\x y -> abs (x - y) / x)
                 (L.concat imgMags)
                 (L.concat predictionMags)) /
            (fromIntegral . getFilterExpansionNum $ filters)
          ratio = (lastErr - err) / lastErr
      if err < 0.000
        then return input
        else do
          let delta =
                L.foldl1' (VU.zipWith (+)) $
                parZipWith3
                  rdeepseq
                  (\im p f
                         -- VU.map realPart .
                         -- L.foldl1' (VU.zipWith (+)) .
                         -- L.zipWith (\a b -> VU.map (\c  -> realPart $ (conjugate c) * b ) a) f $
                         -- L.zipWith
                         --   (\a b ->
                         --       ((a ^ (2 :: Int) - magnitude b ^ (2 :: Int)) :+ 0) *
                         --        b)
                         --   im
                         --   p
                     ->
                      let x =
                            createImagefromR rows cols (div rows 2) (div cols 2) .
                            L.zip
                              (L.map round . pinwheelWaveletRadius . getFilterParams $
                               filters) $
                            L.zipWith
                              (\a b ->
                                  ((a ^ (2 :: Int) - magnitude b ^ (2 :: Int)) :+
                                   0) *
                                  conjugate b)
                              im
                              p
                      in createImagefromRReal
                           rows
                           cols
                           (div rows 2)
                           (div cols 2) .
                         L.zip
                           (L.map round . pinwheelWaveletRadius . getFilterParams $
                            filters) .
                         L.map (realPart . VU.sum . VU.zipWith (*) x) $
                         f)
                  imgMags
                  predictionComplex
                  (L.concatMap L.concat . getFilter $ inverseFilters)
          putStrLn $ show err L.++ " " L.++ show ratio
          go
            (VU.zipWith (+) input . VU.map (\x -> x * learningRate :+ 0) $ delta)
            err
            (timeStep + 1)


-- pinwheelWaveletMagnitudeRecon
--   :: Int
--   -> Int
--   -> Double
--   -> Double
--   -> VU.Vector (Complex Double)
--   -> PinwheelWaveletExpansion
--   -> Int
--   -> IO (VU.Vector (Complex Double))
-- pinwheelWaveletMagnitudeRecon rows cols learningRate threshold img filters writeStep = do
--   prediction <-
--     VU.fromListN (rows * cols) . L.map (:+ 0) <$>
--     M.replicateM (rows * cols) (normalIO' (0, 128))
--   go (normalizeImageU (-1, 1) prediction) (read "Infinity" :: Double) (1 :: Int)
--   where
--     filterList = L.concat . getFilter $ filters
--     cFilterList = L.map (L.map (L.map (VU.map conjugate))) filterList
--     imgMags =
--       L.zipWith
--         (L.zipWith
--            (\x ->
--                VU.map magnitude .
--                L.foldl1' (VU.zipWith (+)) .
--                L.zipWith
--                  (\a b ->
--                      let c = VU.sum $ VU.zipWith (*) a img
--                      in VU.map (* c) b)
--                  x))
--         filterList
--         cFilterList
--     go !input lastErr timeStep = do
--       when
--         (mod timeStep writeStep == 0)
--         (writeImage
--            (show (div timeStep writeStep) L.++ ".pgm")
--            rows
--            cols
--            (VU.convert input))
--       let predictionComplex =
--             L.zipWith
--               (L.zipWith
--                  (\x ->
--                      L.foldl1' (VU.zipWith (+)) .
--                      L.zipWith
--                        (\a b ->
--                            let c = VU.sum $ VU.zipWith (*) a input
--                            in VU.map (* c) b)
--                        x))
--               filterList
--               cFilterList
--           predictionMags = L.map (L.map (VU.map magnitude)) predictionComplex
--           err =
--             L.sum
--               (L.zipWith
--                  (\x y -> (VU.sum . VU.map abs $ VU.zipWith (-) x y) / VU.sum x)
--                  (L.concat imgMags)
--                  (L.concat predictionMags)) /
--             (fromIntegral . getFilterExpansionNum $ filters)
--           ratio = (lastErr - err) / lastErr
--       if err < 0.000
--         then return input
--         else do
--           let delta =
--                 L.foldl1' (VU.zipWith (+)) $
--                 parZipWith3
--                   rdeepseq
--                   (\im p f ->
--                       L.foldl1' (VU.zipWith (+)) .
--                       L.zipWith
--                         (\a b -> VU.map (\c -> realPart $ (conjugate c) * b) a)
--                         f $
--                       L.zipWith
--                         (\a b ->
--                             ((a ^ (2 :: Int) - magnitude b ^ (2 :: Int)) :+ 0) *
--                             b)
--                         im
--                         p)
--                   imgMags
--                   predictionComplex
--                   (L.concatMap L.concat . getFilter $ filters)
--           putStrLn $ show err L.++ " " L.++ show ratio
--           go
--             (VU.zipWith (+) input . VU.map (\x -> x * learningRate :+ 0) $ delta)
--             err
--             (timeStep + 1)


{-# INLINE createImagefromR #-}

createImagefromR
  :: Int
  -> Int
  -> Int
  -> Int
  -> [(Int, Complex Double)]
  -> VU.Vector (Complex Double)
createImagefromR rows cols rCenter cCenter xs =
  VU.fromList $
  makeFilterExpansionList
    rows
    cols
    rCenter
    cCenter
    (linearInterpolation dataVec diffVec)
  where
    dataVec = VU.accum (+) (VU.replicate ((fst . L.last $ xs) + 1) 0) xs
    diffVec =
      VU.generate
        (VU.length dataVec - 1)
        (\i -> dataVec VU.! (i + 1) - dataVec VU.! i)



{-# INLINE linearInterpolation #-}

linearInterpolation
  :: VU.Vector (Complex Double)
  -> VU.Vector (Complex Double)
  -> Int
  -> Int
  -> Complex Double
linearInterpolation dataVec diffVec x y
  | idx == VU.length dataVec - 1 = VU.last dataVec
  | idx < VU.length dataVec - 1 =
    dataVec VU.! idx +
    ((r - fromIntegral idx) :+ (r - fromIntegral idx)) * diffVec VU.! idx
  | otherwise = 0 :+ 0
  where
    r = sqrt . fromIntegral $ x ^ (2 :: Int) + y ^ (2 :: Int)
    idx = floor r


{-# INLINE createImagefromRReal #-}

createImagefromRReal :: Int
                     -> Int
                     -> Int
                     -> Int
                     -> [(Int, Double)]
                     -> VU.Vector Double
createImagefromRReal rows cols rCenter cCenter xs =
  VU.fromList $
  makeFilterExpansionList
    rows
    cols
    rCenter
    cCenter
    (linearInterpolationReal dataVec diffVec)
  where
    dataVec = VU.accum (+) (VU.replicate ((fst . L.last $ xs) + 1) 0) xs
    diffVec =
      VU.generate
        (VU.length dataVec - 1)
        (\i -> dataVec VU.! (i + 1) - dataVec VU.! i)



{-# INLINE linearInterpolationReal #-}

linearInterpolationReal :: VU.Vector Double
                        -> VU.Vector Double
                        -> Int
                        -> Int
                        -> Double
linearInterpolationReal dataVec diffVec x y
  | idx == VU.length dataVec - 1 = VU.last dataVec
  | idx < VU.length dataVec - 1 =
    dataVec VU.! idx + (r - fromIntegral idx) * diffVec VU.! idx
  | otherwise = 0
  where
    r = sqrt . fromIntegral $ x ^ (2 :: Int) + y ^ (2 :: Int)
    idx = floor r
