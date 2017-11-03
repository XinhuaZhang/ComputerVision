{-# LANGUAGE BangPatterns #-}
module CV.Filter.ShiftablePinwheelPyramid
  ( module CV.Utility.FFT
  , ShiftablePinwheelPyramidParams(..)
  , ShiftablePinwheelBlobPyramidParams(..)
  , ShiftablePinwheelRingPyramidFilters(..)
  , ShiftablePinwheelBlobPyramidFilters(..)
  , ShiftablePinwheelRingLogGaborFilters(..)
  , ShiftablePinwheelPyramidInputArray
  , generateShiftablePinwheelRingPyramidFilters
  , shiftablePinwheelRingPyramid
  , generateShiftablePinwheelBlobPyramidFilters
  , shiftablePinwheelBlobPyramid
  , generateShiftablePinwheelRingLogGaborFilters
  , shiftablePinwheelRingLogGabor
  , featureExtractionRing
  , featureExtractionRing1
  , featureExtractionBlob
  , shiftablePinwheel
  , shiftablePinwheelBlobPyramidArray
  , featureExtractionBlobMag
  , generateShiftablePinwheelBlobPyramidFilters1
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

data ShiftablePinwheelRingLogGaborFilters = ShiftablePinwheelRingLogGaborFilters
  { shiftablePinwheelRingLogGaborParams :: ShiftablePinwheelPyramidParams
  , shiftablePinwheelRingLogGaborB      :: [VS.Vector (Complex Double)]
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
  -> ShiftablePinwheelRingPyramidFilters
  -> (ShiftablePinwheelPyramidInternalArray -> [VU.Vector Double])
  -> ShiftablePinwheelPyramidInputArray
  -> IO [[VU.Vector Double]]
shiftablePinwheelRingPyramid fftw (ShiftablePinwheelRingPyramidFilters params@(ShiftablePinwheelPyramidParams _ nCenter nChannel nT nR) h0' l0' h1' l1') f arr = do
  vec <-
    dft1dG fftw [nCenter, nChannel, nT, nR] [2, 3] .
    VS.convert . toUnboxed . computeS . R.map (:+ 0) $
    arr
  let h = VS.zipWith (*) h0' vec
      l = VS.zipWith (*) l0' vec
  hVec <- idft1dG fftw [nCenter, nChannel, nT, nR] [3] h
  let !hFeatures =
        f . fromUnboxed (Z :. nCenter :. nChannel :. nT :. nR) . VS.convert $
        hVec
  lFeatures <- shiftablePinwheelRingPyramidLoop fftw params f 0 h1' l1' l
  return $! hFeatures : lFeatures

{-# INLINE shiftablePinwheelRingPyramidLoop #-}

shiftablePinwheelRingPyramidLoop
  :: FFTW
  -> ShiftablePinwheelPyramidParams
  -> (ShiftablePinwheelPyramidInternalArray -> [VU.Vector Double])
  -> Int
  -> [VS.Vector (Complex Double)]
  -> [VS.Vector (Complex Double)]
  -> VS.Vector (Complex Double)
  -> IO [[VU.Vector Double]]
shiftablePinwheelRingPyramidLoop fftw (ShiftablePinwheelPyramidParams _ nCenter nChannel nT nR) f n [] [] imgVec = do
  vec <- idft1dG fftw [nCenter, nChannel, nT, div nR (2 ^ n)] [3] imgVec
  return
    [ f .
      fromUnboxed (Z :. nCenter :. nChannel :. nT :. div nR (2 ^ n)) .
      VS.convert $
      vec
    ]
shiftablePinwheelRingPyramidLoop fftw params@(ShiftablePinwheelPyramidParams _ nCenter nChannel nT nR) f n (h1':h1s) (l1':l1s) imgVec = do
  let h = VS.zipWith (*) h1' imgVec
      l = VS.zipWith (*) l1' imgVec
  hVec <- idft1dG fftw [nCenter, nChannel, nT, div nR (2 ^ n)] [3] h
  lVec <- idft1dG fftw [nCenter, nChannel, nT, div nR (2 ^ n)] [3] l
  let !hFeatures =
        f .
        fromUnboxed (Z :. nCenter :. nChannel :. nT :. div nR (2 ^ n)) .
        VS.convert $
        hVec
  lArr <-
    computeP .
    downsample [2, 1, 1, 1] .
    fromUnboxed (Z :. nCenter :. nChannel :. nT :. div nR (2 ^ n)) . VS.convert $
    lVec
  downsampledFourierLVec <-
    dft1dG fftw [nCenter, nChannel, nT, div nR (2 ^ (n + 1))] [3] .
    VS.convert . toUnboxed $
    lArr
  features <-
    shiftablePinwheelRingPyramidLoop
      fftw
      params
      f
      (n + 1)
      h1s
      l1s
      downsampledFourierLVec
  return $! hFeatures : features
shiftablePinwheelRingPyramidLoop _ _ _ _ _ _ _ =
  error
    "shiftablePinwheelRingPyramidLoop: The lengths of l1 and h1 lists are not the same."

{-# INLINE featureExtractionRing #-}

featureExtractionRing :: ShiftablePinwheelPyramidInternalArray
                      -> [VU.Vector Double]
featureExtractionRing arr' =
  L.map
    (\(i, k) ->
       toUnboxed . computeS . R.slice magArr $ (Z :. i :. All :. All :. k))
    [(i, k) | i <- [0 .. nCenter - 1], k <- [newR, newR + 1 .. nR - 1]]
  where
    (Z :. nCenter :. _ :. _ :. nR) = extent arr'
    magArr = R.map magnitude arr'
    newR = 0 -- div nR 4
    -- s = sumAllS . R.map (^ (2 :: Int) ) $ magArr
    -- normalizedArr = R.map (/s) magArr

{-# INLINE featureExtractionRing1 #-}

featureExtractionRing1 :: ShiftablePinwheelPyramidInternalArray
                       -> [VU.Vector Double]
featureExtractionRing1 arr' =
  L.map
    (\i -> toUnboxed . computeS . R.slice magArr $ (Z :. i :. All :. All :. All))
    [i | i <- [0 .. nCenter - 1]]
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
  :: FFTW
  -> Int
  -> ShiftablePinwheelBlobPyramidFilters
  -> (Int -> ShiftablePinwheelPyramidInternalArray -> [VU.Vector Double])
  -> ShiftablePinwheelPyramidInputArray
  -> IO [[VU.Vector Double]]
shiftablePinwheelBlobPyramid fftw stride' (ShiftablePinwheelBlobPyramidFilters params@(ShiftablePinwheelBlobPyramidParams _ nCenter nChannel nT nR _) h0' l0' b1' l1') f arr = do
  vec <-
    dft1dG fftw [nCenter, nChannel, nT, nR] [2, 3] .
    VS.convert . toUnboxed . computeS . R.map (:+ 0) $
    arr
  let h = VS.zipWith (*) h0' vec
      l = VS.zipWith (*) l0' vec
  hVec <- idft1dG fftw [nCenter, nChannel, nT, nR] [2, 3] h
  let !hFeatures =
        f stride' .
        fromUnboxed (Z :. nCenter :. nChannel :. nT :. nR) . VS.convert $
        hVec
  lFeatures <-
    shiftablePinwheelBlobRingPyramidLoop fftw stride' params f 0 b1' l1' l
  return $! hFeatures : lFeatures

{-# INLINE shiftablePinwheelBlobRingPyramidLoop #-}

shiftablePinwheelBlobRingPyramidLoop
  :: FFTW
  -> Int
  -> ShiftablePinwheelBlobPyramidParams
  -> (Int -> ShiftablePinwheelPyramidInternalArray -> [VU.Vector Double])
  -> Int
  -> [[VS.Vector (Complex Double)]]
  -> [VS.Vector (Complex Double)]
  -> VS.Vector (Complex Double)
  -> IO [[VU.Vector Double]]
shiftablePinwheelBlobRingPyramidLoop fftw stride' (ShiftablePinwheelBlobPyramidParams _ nCenter nChannel nT nR _) f n [] [] imgVec

 = do
  vec <-
    idft1dG fftw [nCenter, nChannel, div nT (2 ^ n), div nR (2 ^ n)] [2, 3] imgVec
  return
    [ f stride' .
      fromUnboxed (Z :. nCenter :. nChannel :. div nT (2 ^ n) :. div nR (2 ^ n)) .
      VS.convert $
      vec
    ]
shiftablePinwheelBlobRingPyramidLoop fftw stride' params@(ShiftablePinwheelBlobPyramidParams _ nCenter nChannel nT nR _) f n (b1':b1s) (l1':l1s) imgVec = do
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
        parMap
          rdeepseq
          (f stride' .
           fromUnboxed
             (Z :. nCenter :. nChannel :. div nT (2 ^ n) :. div nR (2 ^ n)) .
           VS.convert) $
        hVecs
  lArr <-
    computeP .
    downsample [2, 2, 1, 1] .
    fromUnboxed (Z :. nCenter :. nChannel :. div nT (2 ^ n) :. div nR (2 ^ n)) .
    VS.convert $
    lVec
  downsampledFourierLVec <-
    dft1dG
      fftw
      [nCenter, nChannel, div nT (2 ^ (n + 1)), div nR (2 ^ (n + 1))]
      [2, 3] .
    VS.convert . toUnboxed $
    lArr
  features <-
    shiftablePinwheelBlobRingPyramidLoop
      fftw
      stride'
      params
      f
      (n + 1)
      b1s
      l1s
      downsampledFourierLVec
  return $! hFeatures : features
shiftablePinwheelBlobRingPyramidLoop _ _ _ _ _ _ _ _ = error "shiftablePinwheelBlobRingPyramidLoop: The lengths of l1 and h1 lists are not the same."


{-# INLINE featureExtractionBlob #-}

featureExtractionBlob
  :: Int
  -> ShiftablePinwheelPyramidInternalArray
  -> [VU.Vector Double]
featureExtractionBlob stride' arr' =
  L.map
    (\(i, j, k) ->
       toUnboxed . computeS . R.slice magArr $ (Z :. i :. All :. j :. k))
    [ (i, j, k)
    | i <- [0 .. nCenter - 1]
    , j <- [0,stride' .. nT - 1]
    , k <- [newR,newR + stride' .. nR - 1]
    ]
  where
    (Z :. nCenter :. _ :. nT :. nR) = extent arr'
    magArr = R.map magnitude arr'
    newR = div nR 4

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
        -- crop [0,0,0,0] [div nR 4, div nT 4, numChannels, numCenters] .
        fromUnboxed (Z :. numCenters :. numChannels :. nT :. nR) .
        VS.convert . VS.map magnitude $
        vec
  return
    [ l2norm . toUnboxed . computeS . R.slice arr1 $
    (Z :. i :. All :. All :. All)
    | i <- [0 .. numCenters - 1]
    ]



generateShiftablePinwheelRingLogGaborFilters :: ShiftablePinwheelPyramidParams
                                             -> ShiftablePinwheelRingLogGaborFilters
generateShiftablePinwheelRingLogGaborFilters params@(ShiftablePinwheelPyramidParams nLayer nCenter nChannel nT nR) =
  ShiftablePinwheelRingLogGaborFilters params .
  L.map (makeFilter (logGabor nLayer)) $
  [1 .. nLayer]
  where
    makeFilter op s =
      VS.convert .
      toUnboxed . computeS . fromFunction (Z :. nCenter :. nChannel :. nT :. nR) $
      (\(Z :. _ :. _ :. _ :. i) -> op s . fftwFreqOpBlob nR $ i)

{-# INLINE shiftablePinwheelRingLogGabor #-}

shiftablePinwheelRingLogGabor
  :: FFTW
  -> ShiftablePinwheelRingLogGaborFilters
  -> (ShiftablePinwheelPyramidInternalArray -> [VU.Vector Double])
  -> ShiftablePinwheelPyramidInputArray
  -> IO [[VU.Vector Double]]
shiftablePinwheelRingLogGabor fftw (ShiftablePinwheelRingLogGaborFilters (ShiftablePinwheelPyramidParams _ nCenter nChannel nT nR) filters) f arr = do
  imgVec <-
    dft1dG fftw [nCenter, nChannel, nT, nR] [2, 3] .
    VS.convert . toUnboxed . computeS . R.map (:+ 0) $
    arr
  let filteredImageFourierVecs = parMap rdeepseq (VS.zipWith (*) imgVec) filters
  filteredImageVecs <-
    M.mapM
      (idft1dG fftw [nCenter, nChannel, nT, nR] [3])
      filteredImageFourierVecs
  -- let results =
  --       [ L.map VU.concat .
  --         L.transpose .
  --         parMap
  --           rdeepseq
  --           (f . fromUnboxed (Z :. nCenter :. nChannel :. nT :. nR) . VS.convert) $
  --         filteredImageVecs
  --       ]
  -- return results
  return .
    parMap
      rdeepseq
      (f . fromUnboxed (Z :. nCenter :. nChannel :. nT :. nR) . VS.convert) $
    filteredImageVecs

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
  | x <= pi / n = 0
  | otherwise = cos (0.5 * pi * logBase (n / 2) (2 * x / pi)) :+ 0
  where
    n = 4

{-# INLINE l1 #-}

l1 :: Double -> Complex Double
l1 x
  | x >= 0.5 * pi = 0
  -- | x <= 0 = 0
  | x <= pi / n = 2
  | otherwise = (2 * cos 0.5 * pi * logBase (n / 2) (n * x / pi)) :+ 0
  where
    n = 4

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

{-# INLINE l2norm #-}

l2norm :: VU.Vector Double -> VU.Vector Double
l2norm vec
  | VU.null vec = error "l2norm: empty vector."
  | VU.all (== 0) vec = vec
  | otherwise = VU.map (/ norm) vec
  where
    norm = sqrt . VU.sum . VU.map (^ (2 :: Int)) $ vec

{-# INLINE logGabor #-}

logGabor :: Int -> Int -> Double -> Complex Double
logGabor n s r
  | r <= 0 = 0
  | otherwise = (exp (-0.5 * (((rho - rhoS) / sigma) ^ (2 :: Int)))) :+ 0
  where
    rho = logBase 2 r
    rhoS = logBase 2 (fromIntegral n) - fromIntegral s
    sigma = 0.996 * sqrt (2 / 3)



-- The dimension of array is NumChannels X NumCenters X NumTheta X NumLogR

{-# INLINE shiftablePinwheelBlobPyramidArray #-}

shiftablePinwheelBlobPyramidArray
  :: FFTW
  -> ShiftablePinwheelBlobPyramidFilters
  -> ShiftablePinwheelPyramidInputArray
  -> IO [ShiftablePinwheelPyramidInputArray]
shiftablePinwheelBlobPyramidArray fftw (ShiftablePinwheelBlobPyramidFilters params@(ShiftablePinwheelBlobPyramidParams _ nCenter nChannel nT nR _) h0' l0' b1' l1') arr = do
  vec <-
    dft1dG fftw [nChannel, nCenter, nT, nR] [2, 3] .
    VS.convert . toUnboxed . computeS . R.map (:+ 0) $
    arr
  let h = VS.zipWith (*) h0' vec
      l = VS.zipWith (*) l0' vec
  hVec <- idft1dG fftw [nChannel, nCenter, nT, nR] [2, 3] h
  hFeatures <-
    computeP .
    R.map magnitude .
    fromUnboxed (Z :. nChannel :. nCenter :. nT :. nR) . VS.convert $
    hVec
  lFeatures <- shiftablePinwheelBlobRingPyramidLoopArray fftw params 0 b1' l1' l
  return $! hFeatures : lFeatures 
  

{-# INLINE shiftablePinwheelBlobRingPyramidLoopArray #-}

shiftablePinwheelBlobRingPyramidLoopArray
  :: FFTW
  -> ShiftablePinwheelBlobPyramidParams
  -> Int
  -> [[VS.Vector (Complex Double)]]
  -> [VS.Vector (Complex Double)]
  -> VS.Vector (Complex Double)
  -> IO [ShiftablePinwheelPyramidInputArray]
shiftablePinwheelBlobRingPyramidLoopArray fftw (ShiftablePinwheelBlobPyramidParams _ nCenter nChannel nT nR _) n [] [] imgVec = do
  vec <-
    idft1dG
      fftw
      [nChannel, nCenter, div nT (2 ^ n), div nR (2 ^ n)]
      [2, 3]
      imgVec
  magArr <-
    computeP .
    R.map magnitude .
    fromUnboxed (Z :. nChannel :. nCenter :. div nT (2 ^ n) :. div nR (2 ^ n)) .
    VS.convert $
    vec
  return [magArr]
shiftablePinwheelBlobRingPyramidLoopArray fftw params@(ShiftablePinwheelBlobPyramidParams _ nCenter nChannel nT nR _) n (b1':b1s) (l1':l1s) imgVec = do
  let bs = L.map (VS.zipWith (*) imgVec) b1'
      l = VS.zipWith (*) l1' imgVec
  hVecs <-
    M.mapM
      (idft1dG fftw [nChannel, nCenter, div nT (2 ^ n), div nR (2 ^ n)] [2, 3])
      bs
  lVec <-
    idft1dG fftw [nChannel, nCenter, div nT (2 ^ n), div nR (2 ^ n)] [2, 3] l
  let hFeatureVec =
        fromUnboxed
          (Z :. (nChannel * L.length hVecs) :. nCenter :. div nT (2 ^ n) :.
           div nR (2 ^ n)) .
        VS.convert . VS.concat $
        hVecs
  hFeatures <- computeP . R.map magnitude $ hFeatureVec
  lArr <-
    computeP .
    downsample [2, 2, 1, 1] .
    fromUnboxed (Z :. nChannel :. nCenter :. div nT (2 ^ n) :. div nR (2 ^ n)) .
    VS.convert $
    lVec
  downsampledFourierLVec <-
    dft1dG
      fftw
      [nChannel, nCenter, div nT (2 ^ (n + 1)), div nR (2 ^ (n + 1))]
      [2, 3] .
    VS.convert . toUnboxed $
    lArr
  features <-
    shiftablePinwheelBlobRingPyramidLoopArray
      fftw
      params
      (n + 1)
      b1s
      l1s
      downsampledFourierLVec
  return $! hFeatures : features
shiftablePinwheelBlobRingPyramidLoopArray _ _ _ _ _ _ =
  error
    "shiftablePinwheelBlobRingPyramidLoopArray: The lengths of l1 and h1 lists are not the same."


{-# INLINE featureExtractionBlobMag #-}

featureExtractionBlobMag
  :: Int
  -> ShiftablePinwheelPyramidInputArray
  -> [VU.Vector Double]
featureExtractionBlobMag stride' arr' =
  L.map
    (\(i, j, k) ->
       toUnboxed . computeS . R.slice arr' $ (Z :. All :. i :. j :. k))
    [ (i, j, k)
    | i <- [0 .. nCenter - 1]
    , j <- [0,stride' .. nT - 1]
    , k <- [newR,newR + stride' .. nR - 1]
    ]
  where
    (Z :. _ :. nCenter :. nT :. nR) = extent arr'
    newR = 0 -- div nR 4


generateShiftablePinwheelBlobPyramidFilters1 :: ShiftablePinwheelBlobPyramidParams
                                             -> ShiftablePinwheelBlobPyramidFilters
generateShiftablePinwheelBlobPyramidFilters1 params@(ShiftablePinwheelBlobPyramidParams nLayer nCenter nChannel nT nR k)
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
      computeS . fromFunction (Z  :. nChannel :. nCenter :. nT'' :. nR'') $
      (\(Z :. _ :. _ :. j :. i) ->
          let a = fftwFreqOpBlob nR'' i
              b = fftwFreqOpBlob nT'' j
              r = sqrt $ a ^ (2 :: Int) + b ^ (2 :: Int)
          in op r)
    makeBandpassFilter nT' nR' opR opT =
      VS.convert .
      toUnboxed .
      computeS . fromFunction (Z  :. nChannel :. nCenter :. nT' :. nR') $
      (\(Z :. _ :. _ :. tInd :. rInd) ->
          let a = fftwFreqOpBlob nR' rInd
              b = fftwFreqOpBlob nT' tInd
              r = sqrt $ a ^ (2 :: Int) + b ^ (2 :: Int)
              t = angleFunctionRad a b
          in opT t * opR r)
