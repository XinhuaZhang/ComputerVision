{-# LANGUAGE BangPatterns #-}
module CV.Filter.ShiftablePinwheelPyramid
  ( module CV.Utility.FFT
  , ShiftablePinwheelPyramidParams(..)
  , ShiftablePinwheelBlobPyramidParams(..)
  , ShiftablePinwheelRingPyramidFilters(..)
  , ShiftablePinwheelBlobPyramidFilters(..)
  , ShiftablePinwheelPyramidInputArray
  , generateShiftablePinwheelRingPyramidFilters
  , shiftablePinwheelRingPyramid
  , generateShiftablePinwheelBlobPyramidFilters
  , shiftablePinwheelBlobPyramid
  , featureExtractionRing
  , featureExtractionBlob
  , shiftablePinwheel
  ) where

import           Control.Monad               as M
import           CV.Utility.Coordinates
import           CV.Utility.FFT
import           CV.Utility.Parallel
import           CV.Utility.RepaArrayUtility
import           Data.Array.Repa             as R
import           Data.Complex
import           Data.List                   as L
import           Data.Vector.Storable        as VS
import           Data.Vector.Unboxed         as VU

data ShiftablePinwheelPyramidParams = ShiftablePinwheelPyramidParams
  { shiftablePinwheelPyramidNumLayers   :: Int
  , shiftablePinwheelPyramidNumCenters  :: Int
  , shiftablePinwheelPyramidNumChannels :: Int
  , shiftablePinwheelPyramidNumTheta    :: Int
  , shiftablePinwheelPyramidNumLogR     :: Int
  } deriving (Show,Read)

data ShiftablePinwheelBlobPyramidParams = ShiftablePinwheelBlobPyramidParams
  { shiftablePinwheelBlobPyramidNumLayers   :: Int
  , shiftablePinwheelBlobPyramidNumCenters  :: Int
  , shiftablePinwheelBlobPyramidNumChannels :: Int
  , shiftablePinwheelBlobPyramidNumTheta    :: Int
  , shiftablePinwheelBlobPyramidNumLogR     :: Int
  , shiftablePinwheelBlobPyramidK           :: Int
  } deriving (Show,Read)


-- NumCenters X NumChannels X NumTheta X NumLogR
type ShiftablePinwheelPyramidInputArray  = R.Array U DIM4 Double
type ShiftablePinwheelPyramidInternalArray  = R.Array U DIM4 (Complex Double)

data ShiftablePinwheelRingPyramidFilters = ShiftablePinwheelRingPyramidFilters
  { shiftablePinwheelRingPyramidParams :: ShiftablePinwheelPyramidParams
  , shiftablePinwheelRingPyramidH0     :: VS.Vector (Complex Double)
  , shiftablePinwheelRingPyramidL0     :: VS.Vector (Complex Double)
  , shiftablePinwheelRingPyramidH1     :: [VS.Vector (Complex Double)]
  , shiftablePinwheelRingPyramidL1     :: [VS.Vector (Complex Double)]
  }

data ShiftablePinwheelBlobPyramidFilters = ShiftablePinwheelBlobPyramidFilters
  { shiftablePinwheelBlobPyramidParams :: ShiftablePinwheelBlobPyramidParams
  , shiftablePinwheelBlobPyramidH0     :: VS.Vector (Complex Double)
  , shiftablePinwheelBlobPyramidL0     :: VS.Vector (Complex Double)
  , shiftablePinwheelBlobPyramidB      :: [[VS.Vector (Complex Double)]]
  , shiftablePinwheelBlobPyramidL1     :: [VS.Vector (Complex Double)]
  }

generateShiftablePinwheelRingPyramidFilters :: ShiftablePinwheelPyramidParams
                                            -> ShiftablePinwheelRingPyramidFilters
generateShiftablePinwheelRingPyramidFilters params@(ShiftablePinwheelPyramidParams nLayer nCenter nChannel nT nR)
  | logBase (2 :: Double) (fromIntegral nR) < fromIntegral nLayer =
    error
      "generateShiftablePinwheelRingPyramidFilters: There are too many layers."
  | otherwise =
    ShiftablePinwheelRingPyramidFilters
      params
      (makeFilter nR h0)
      (makeFilter nR l0)
      (L.map (\i -> makeFilter (div nR (2 ^ i)) h1) [0 .. nLayer - 1])
      (L.map (\i -> makeFilter (div nR (2 ^ i)) l1) [0 .. nLayer - 1])
  where
    makeFilter n op =
      VS.convert .
      toUnboxed . computeS . fromFunction (Z :. nCenter :. nChannel :. nT :. n) $
      (\(Z :. _ :. _ :. _ :. i) -> op . fftwFreqOp n $ i)

shiftablePinwheelRingPyramid
  :: FFTW
  -> ParallelParams
  -> ShiftablePinwheelRingPyramidFilters
  -> (ParallelParams -> ShiftablePinwheelPyramidInternalArray -> [VU.Vector Double])
  -> ShiftablePinwheelPyramidInputArray
  -> IO [[VU.Vector Double]]
shiftablePinwheelRingPyramid fftw parallelParams (ShiftablePinwheelRingPyramidFilters params@(ShiftablePinwheelPyramidParams _ nCenter nChannel nT nR) h0' l0' h1' l1') f arr = do
  vec <-
    dft1dG fftw [nCenter, nChannel, nT, nR] [2, 3] .
    VS.convert . toUnboxed . computeS . R.map (:+ 0) $
    arr
  let h = VS.zipWith (*) h0' vec
      l = VS.zipWith (*) l0' vec
  hVec <- idft1dG fftw [nCenter, nChannel, nT, nR] [3] h
  let !hFeatures =
        f parallelParams .
        fromUnboxed (Z :. nCenter :. nChannel :. nT :. nR) . VS.convert $
        hVec
  lFeatures <-
    shiftablePinwheelRingPyramidLoop fftw parallelParams params f 0 h1' l1' l
  return $! hFeatures : lFeatures

{-# INLINE shiftablePinwheelRingPyramidLoop #-}

shiftablePinwheelRingPyramidLoop
  :: FFTW
  -> ParallelParams
  -> ShiftablePinwheelPyramidParams
  -> (ParallelParams -> ShiftablePinwheelPyramidInternalArray -> [VU.Vector Double])
  -> Int
  -> [VS.Vector (Complex Double)]
  -> [VS.Vector (Complex Double)]
  -> VS.Vector (Complex Double)
  -> IO [[VU.Vector Double]]
shiftablePinwheelRingPyramidLoop fftw parallelParams (ShiftablePinwheelPyramidParams _ nCenter nChannel nT nR) f n [] [] imgVec = do
  vec <- idft1dG fftw [nCenter, nChannel, nT, div nR (2 ^ n)] [3] imgVec
  return
    [ f parallelParams .
      fromUnboxed (Z :. nCenter :. nChannel :. nT :. div nR (2 ^ n)) . VS.convert $
      vec
    ]
shiftablePinwheelRingPyramidLoop fftw parallelParams params@(ShiftablePinwheelPyramidParams _ nCenter nChannel nT nR) f n (h1':h1s) (l1':l1s) imgVec = do
  let h = VS.zipWith (*) h1' imgVec
      l = VS.zipWith (*) l1' imgVec
  hVec <- idft1dG fftw [nCenter, nChannel, nT, div nR (2 ^ n)] [3] h
  lVec <- idft1dG fftw [nCenter, nChannel, nT, div nR (2 ^ n)] [3] l
  let !hFeatures =
        f parallelParams .
        fromUnboxed (Z :. nCenter :. nChannel :. nT :. div nR (2 ^ n)) . VS.convert $
        hVec
      lArr =
        downsample [2, 1, 1, 1] .
        fromUnboxed (Z :. nCenter :. nChannel :. nT :. div nR (2 ^ n)) . VS.convert $
        lVec
  downsampledFourierLVec <-
    dft1dG fftw [nCenter, nChannel, nT, div nR (2 ^ (n + 1))] [3] .
    VS.convert . toUnboxed . computeS $
    lArr
  features <-
    shiftablePinwheelRingPyramidLoop
      fftw
      parallelParams
      params
      f
      (n + 1)
      h1s
      l1s
      downsampledFourierLVec
  return $! hFeatures : features
shiftablePinwheelRingPyramidLoop _ _ _ _ _ _ _ _ =
  error
    "shiftablePinwheelRingPyramidLoop: The lengths of l1 and h1 lists are not the same."

{-# INLINE featureExtractionRing #-}

featureExtractionRing :: ParallelParams
                      -> ShiftablePinwheelPyramidInternalArray
                      -> [VU.Vector Double]
featureExtractionRing parallelParams arr' =
  parMapChunk
    parallelParams
    rdeepseq
    (\(i, k) ->
        toUnboxed . computeS . R.slice magArr $ (Z :. i :. All :. All :. k))
    [ (i, k)
    | i <- [0 .. nCenter - 1]
    , k <- [0 .. nR - 1] ]
  where
    (Z :. nCenter :. _ :. _ :. nR) = extent arr'
    magArr = R.map magnitude arr'


generateShiftablePinwheelBlobPyramidFilters :: ShiftablePinwheelBlobPyramidParams
                                            -> ShiftablePinwheelBlobPyramidFilters
generateShiftablePinwheelBlobPyramidFilters params@(ShiftablePinwheelBlobPyramidParams nLayer nCenter nChannel nT nR k)
  | logBase (2 :: Double) (fromIntegral nR) < fromIntegral nLayer =
    error
      "generateShiftablePinwheelBlobPyramidFilters: There are too many layers."
  | otherwise =
    ShiftablePinwheelBlobPyramidFilters
      params
      (makeFilter nT nR h0)
      (makeFilter nT nR l0)
      (L.map
         (\i ->
             L.map
               (makeBandpassFilter (div nT (2 ^ i)) (div nR (2 ^ i)) h1 . g1 k)
               [0 .. k - 1])
         [0 .. nLayer - 1])
      (L.map
         (\i -> makeFilter (div nT (2 ^ i)) (div nR (2 ^ i)) l1)
         [0 .. nLayer - 1])
  where
    makeFilter :: Int
               -> Int
               -> (Double -> Complex Double)
               -> VS.Vector (Complex Double)
    makeFilter nT'' nR'' op =
      VS.convert .
      toUnboxed .
      computeS . fromFunction (Z :. nCenter :. nChannel :. nT'' :. nR'') $
      (\(Z :. _ :. _ :. j :. i) ->
          let a = fftwFreqOpBlob nR'' i
              b = fftwFreqOpBlob nT'' j
              r = sqrt $ a ^ (2 :: Int) + b ^ (2 :: Int)
          in op r)
    makeBandpassFilter nT' nR' opR opT =
      VS.convert .
      toUnboxed .
      computeS . fromFunction (Z :. nCenter :. nChannel :. nT' :. nR') $
      (\(Z :. _ :. _ :. tInd :. rInd) ->
          let a = fftwFreqOpBlob nR' rInd
              b = fftwFreqOpBlob nT' tInd
              r = sqrt $ a ^ (2 :: Int) + b ^ (2 :: Int)
              t = angleFunctionRad a b
          in opT t * opR r)

shiftablePinwheelBlobPyramid
  :: ParallelParams
  -> FFTW
  -> ShiftablePinwheelBlobPyramidFilters
  -> (ParallelParams -> ShiftablePinwheelPyramidInternalArray -> [VU.Vector Double])
  -> ShiftablePinwheelPyramidInputArray
  -> IO [[VU.Vector Double]]
shiftablePinwheelBlobPyramid parallelParams fftw (ShiftablePinwheelBlobPyramidFilters params@(ShiftablePinwheelBlobPyramidParams _ nCenter nChannel nT nR _) h0' l0' b1' l1') f arr = do
  vec <-
    dft1dG fftw [nCenter, nChannel, nT, nR] [2, 3] .
    VS.convert . toUnboxed . computeS . R.map (:+ 0) $
    arr
  let h = VS.zipWith (*) h0' vec
      l = VS.zipWith (*) l0' vec
  hVec <- idft1dG fftw [nCenter, nChannel, nT, nR] [2, 3] h
  let !hFeatures =
        f parallelParams .
        fromUnboxed (Z :. nCenter :. nChannel :. nT :. nR) . VS.convert $
        hVec
  lFeatures <-
    shiftablePinwheelBlobRingPyramidLoop
      parallelParams
      fftw
      params
      f
      0
      b1'
      l1'
      l
  return $! hFeatures : lFeatures

{-# INLINE shiftablePinwheelBlobRingPyramidLoop #-}

shiftablePinwheelBlobRingPyramidLoop
  :: ParallelParams
  -> FFTW
  -> ShiftablePinwheelBlobPyramidParams
  -> (ParallelParams -> ShiftablePinwheelPyramidInternalArray -> [VU.Vector Double])
  -> Int
  -> [[VS.Vector (Complex Double)]]
  -> [VS.Vector (Complex Double)]
  -> VS.Vector (Complex Double)
  -> IO [[VU.Vector Double]]
shiftablePinwheelBlobRingPyramidLoop parallelParams fftw (ShiftablePinwheelBlobPyramidParams _ nCenter nChannel nT nR _) f n [] [] imgVec = do
  vec <-
    idft1dG fftw [nCenter, nChannel, div nT (2 ^ n), div nR (2 ^ n)] [2, 3] imgVec
  return
    [ f parallelParams .
      fromUnboxed (Z :. nCenter :. nChannel :. div nT (2 ^ n) :. div nR (2 ^ n)) .
      VS.convert $
      vec
    ]
shiftablePinwheelBlobRingPyramidLoop parallelParams fftw params@(ShiftablePinwheelBlobPyramidParams _ nCenter nChannel nT nR _) f n (b1':b1s) (l1':l1s) imgVec = do
  let bs = L.map (VS.zipWith (*) imgVec) b1'
      l = VS.zipWith (*) l1' imgVec
  hVecs <-
    M.mapM
      (idft1dG fftw [nCenter, nChannel, div nT (2 ^ n), div nR (2 ^ n)] [2, 3])
      bs
  lVec <-
    idft1dG fftw [nCenter, nChannel, div nT (2 ^ n), div nR (2 ^ n)] [2, 3] l
  let !hFeatures =
        L.map VU.concat .
        L.transpose .
        L.map
          (f parallelParams .
           fromUnboxed
             (Z :. nCenter :. nChannel :. div nT (2 ^ n) :. div nR (2 ^ n)) .
           VS.convert) $
        hVecs
      lArr =
        downsample [2, 2, 1, 1] .
        fromUnboxed
          (Z :. nCenter :. nChannel :. div nT (2 ^ n) :. div nR (2 ^ n)) .
        VS.convert $
        lVec
  downsampledFourierLVec <-
    dft1dG
      fftw
      [nCenter, nChannel, div nT (2 ^ (n + 1)), div nR (2 ^ (n + 1))]
      [2, 3] .
    VS.convert . toUnboxed . computeS $
    lArr
  features <-
    shiftablePinwheelBlobRingPyramidLoop
      parallelParams
      fftw
      params
      f
      (n + 1)
      b1s
      l1s
      downsampledFourierLVec
  return $! hFeatures : features
shiftablePinwheelBlobRingPyramidLoop _ _ _ _ _ _ _ _ = error "shiftablePinwheelBlobRingPyramidLoop: The lengths of l1 and h1 lists are not the same."


{-# INLINE featureExtractionBlob #-}

featureExtractionBlob :: ParallelParams
                      -> ShiftablePinwheelPyramidInternalArray
                      -> [VU.Vector Double]
featureExtractionBlob parallelParams arr' =
  parMapChunk
    parallelParams
    rdeepseq
    (\(i, j, k) ->
        toUnboxed . computeS . R.slice magArr $ (Z :. i :. All :. j :. k))
    [ (i, j, k)
    | i <- [0 .. nCenter - 1]
    , j <- [0 .. nT - 1]
    , k <- [0 .. nR - 1] ]
  where
    (Z :. nCenter :. _ :. nT :. nR) = extent arr'
    magArr = R.map magnitude arr'
    
shiftablePinwheel :: FFTW
                  -> Int
                  -> ShiftablePinwheelPyramidInputArray
                  -> IO [VU.Vector Double]
shiftablePinwheel fftw downsampleFactor arr = do
  let (Z :. numCenters :. numChannels :. nT :. nR) = extent arr
  vec <-
    dft1dG fftw [numCenters, numChannels, nT, nR] [2, 3] .
    VS.convert . toUnboxed . computeS . R.map (:+ 0) $
    arr
  let arr1 =
        downsample [downsampleFactor, downsampleFactor, 1, 1] .
        fromUnboxed
          (Z :. numCenters :. numChannels :. div nT downsampleFactor :. div nR downsampleFactor) .
        VS.convert . VS.map magnitude $
        vec
  return
    [ toUnboxed . computeS . R.slice arr1 $ (Z :. i :. All :. All :. All)
    | i <- [0 .. numCenters - 1] ]



{-# INLINE fftwFreqOp #-}

fftwFreqOp :: Int -> Int -> Double
fftwFreqOp n i
  | i <= div n 2 = 2 * pi * fromIntegral i / fromIntegral n
  | otherwise = 2 * pi * fromIntegral (n - i) / fromIntegral n
  
{-# INLINE fftwFreqOpBlob #-}

fftwFreqOpBlob :: Int -> Int -> Double
fftwFreqOpBlob n i
  | i <= div n 2 = 2 * pi * fromIntegral i / fromIntegral n
  | otherwise = 2 * pi * fromIntegral (i - n) / fromIntegral n

{-# INLINE h1 #-}

h1 :: Double -> Complex Double
h1 x
  | x >= 0.5 * pi = 1
  | x <= 0.25 * pi = 0
  | otherwise = cos (0.5 * pi * logBase 2 (2 * x / pi)) :+ 0

{-# INLINE l1 #-}

l1 :: Double -> Complex Double
l1 x
  | x >= 0.5 * pi = 0
  | x <= 0.25 * pi = 2
  | otherwise = (2 * cos 0.5 * pi * logBase 2 (4 * x / pi)) :+ 0

{-# INLINE h0 #-}

h0 :: Double -> Complex Double
h0 x = h1 (x / 2)

{-# INLINE l0  #-}

l0 :: Double -> Complex Double
l0 x = l1 (x / 2) / 2

{-# INLINE g1 #-}

g1 :: Int -> Int -> Double -> Complex Double
g1 num k theta
  | abs (theta - pi * fromIntegral k / fromIntegral num) < 0.5 * pi =
    (alpha * (cos (theta - pi * fromIntegral k / fromIntegral num)) ^ (num - 1)) :+
    0
  | otherwise = 0
  where
    alpha =
      ((2 :: Double) ^ (num - 1)) * (fromIntegral . L.product $ [2 .. num - 1]) /
      sqrt (fromIntegral $ num * L.product [2 .. 2 * (num - 1)])
