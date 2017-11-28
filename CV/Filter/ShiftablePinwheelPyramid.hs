{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
module CV.Filter.ShiftablePinwheelPyramid
  ( module DFT
  , ShiftablePinwheelPyramidParams(..)
  , ShiftablePinwheelBlobPyramidParams(..)
  , ShiftablePinwheelRingPyramidFilters(..)
  , ShiftablePinwheelBlobPyramidFilters(..)
  , ShiftablePinwheelRingLogGaborFilters(..)
  , ShiftablePinwheelPyramidInputArray
  , generateShiftablePinwheelRingPyramidFilters
  -- , shiftablePinwheelRingPyramid
  , generateShiftablePinwheelBlobPyramidFilters
  , shiftablePinwheelBlobPyramidPlan
  , shiftablePinwheelBlobPyramid
  , shiftablePinwheelBlobPyramidDFT
  , generateShiftablePinwheelRingLogGaborFilters
  -- , shiftablePinwheelRingLogGabor
  , featureExtractionRing
  , featureExtractionRing1
  , featureExtractionBlob
  , featureExtractionBlobDFT
  -- , shiftablePinwheel
  -- , shiftablePinwheelBlobPyramidArray
  , featureExtractionBlobMag
  , generateShiftablePinwheelBlobPyramidFilters1
  , shiftablePinwheelBlobPyramidScatteringNetworks
  , ShiftablePinwheelBlobPyramidScatteringNetworksFilters(..)
  , generateShiftablePinwheelBlobPyramidScatteringNetworksFilters
  , featureExtractionBlobScattering
  , shiftablePinwheelBlobPyramidScatteringNetworksPlan
  ) where

import           Control.DeepSeq
import           Control.Monad               as M
import           CV.Utility.Coordinates
import           CV.Utility.DFT              as DFT
import           CV.Utility.Parallel
import           CV.Utility.RepaArrayUtility
import           Data.Array.Repa             as R
import           Data.Complex
import           Data.Hashable
import           Data.List                   as L
import           Data.Vector.Storable        as VS
import           Data.Vector.Unboxed         as VU
import           GHC.Generics                (Generic)

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
  } deriving (Show, Read, Eq, Generic)


-- NumCenters X NumChannels X NumTheta X NumLogR
type ShiftablePinwheelPyramidInputArray  = R.Array U DIM4 Double
type ShiftablePinwheelPyramidInternalArray  = R.Array U DIM4 (Complex Double)

instance NFData ShiftablePinwheelPyramidInputArray where
  rnf arr = deepSeqArray arr ()

instance NFData ShiftablePinwheelPyramidInternalArray where
  rnf arr = deepSeqArray arr ()

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

type ShiftablePinwheelBlobPyramidScatteringNetworksFilters = HashMap ShiftablePinwheelBlobPyramidParams ShiftablePinwheelBlobPyramidFilters

instance Hashable ShiftablePinwheelBlobPyramidParams

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


-- shiftablePinwheelRingPyramid
--   :: FFTW
--   -> ShiftablePinwheelRingPyramidFilters
--   -> (ShiftablePinwheelPyramidInternalArray -> [VU.Vector Double])
--   -> ShiftablePinwheelPyramidInputArray
--   -> IO [[VU.Vector Double]]
-- shiftablePinwheelRingPyramid fftw (ShiftablePinwheelRingPyramidFilters params@(ShiftablePinwheelPyramidParams _ nCenter nChannel nT nR) h0' l0' h1' l1') f arr = do
--   vec <-
--     dft1dG fftw [nCenter, nChannel, nT, nR] [2, 3] .
--     VS.convert . toUnboxed . computeS . R.map (:+ 0) $
--     arr
--   let h = VS.zipWith (*) h0' vec
--       l = VS.zipWith (*) l0' vec
--   hVec <- idft1dG fftw [nCenter, nChannel, nT, nR] [3] h
--   let !hFeatures =
--         f . fromUnboxed (Z :. nCenter :. nChannel :. nT :. nR) . VS.convert $
--         hVec
--   lFeatures <- shiftablePinwheelRingPyramidLoop fftw params f 0 h1' l1' l
--   return $! hFeatures : lFeatures

-- {-# INLINE shiftablePinwheelRingPyramidLoop #-}

-- shiftablePinwheelRingPyramidLoop
--   :: FFTW
--   -> ShiftablePinwheelPyramidParams
--   -> (ShiftablePinwheelPyramidInternalArray -> [VU.Vector Double])
--   -> Int
--   -> [VS.Vector (Complex Double)]
--   -> [VS.Vector (Complex Double)]
--   -> VS.Vector (Complex Double)
--   -> IO [[VU.Vector Double]]
-- shiftablePinwheelRingPyramidLoop fftw (ShiftablePinwheelPyramidParams _ nCenter nChannel nT nR) f n [] [] imgVec = do
--   vec <- idft1dG fftw [nCenter, nChannel, nT, div nR (2 ^ n)] [3] imgVec
--   return
--     [ f .
--       fromUnboxed (Z :. nCenter :. nChannel :. nT :. div nR (2 ^ n)) .
--       VS.convert $
--       vec
--     ]
-- shiftablePinwheelRingPyramidLoop fftw params@(ShiftablePinwheelPyramidParams _ nCenter nChannel nT nR) f n (h1':h1s) (l1':l1s) imgVec = do
--   let h = VS.zipWith (*) h1' imgVec
--       l = VS.zipWith (*) l1' imgVec
--   hVec <- idft1dG fftw [nCenter, nChannel, nT, div nR (2 ^ n)] [3] h
--   lVec <- idft1dG fftw [nCenter, nChannel, nT, div nR (2 ^ n)] [3] l
--   let !hFeatures =
--         f .
--         fromUnboxed (Z :. nCenter :. nChannel :. nT :. div nR (2 ^ n)) .
--         VS.convert $
--         hVec
--   lArr <-
--     computeP .
--     downsample [2, 1, 1, 1] .
--     fromUnboxed (Z :. nCenter :. nChannel :. nT :. div nR (2 ^ n)) . VS.convert $
--     lVec
--   downsampledFourierLVec <-
--     dft1dG fftw [nCenter, nChannel, nT, div nR (2 ^ (n + 1))] [3] .
--     VS.convert . toUnboxed $
--     lArr
--   features <-
--     shiftablePinwheelRingPyramidLoop
--       fftw
--       params
--       f
--       (n + 1)
--       h1s
--       l1s
--       downsampledFourierLVec
--   return $! hFeatures : features
-- shiftablePinwheelRingPyramidLoop _ _ _ _ _ _ _ =
--   error
--     "shiftablePinwheelRingPyramidLoop: The lengths of l1 and h1 lists are not the same."

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

shiftablePinwheelBlobPyramidPlan
  :: DFTPlan
  -> ShiftablePinwheelBlobPyramidFilters
  -> ShiftablePinwheelPyramidInputArray
  -> IO DFTPlan
shiftablePinwheelBlobPyramidPlan plan (ShiftablePinwheelBlobPyramidFilters params@(ShiftablePinwheelBlobPyramidParams _ nCenter nChannel nT nR _) h0' l0' b1' l1') arr = do
  lock <- getFFTWLock
  (p1, vec) <-
    dft1dGPlan lock plan [nCenter, nChannel, nT, nR] [2, 3] .
    VS.convert . toUnboxed . computeS . R.map (:+ 0) $
    arr
  let h = VS.zipWith (*) h0' vec
      l = VS.zipWith (*) l0' vec
  (p2, _) <- idft1dGPlan lock p1 [nCenter, nChannel, nT, nR] [2, 3] h
  shiftablePinwheelBlobPyramidLoopPlan lock p2 params 0 b1' l1' l

{-# INLINE shiftablePinwheelBlobPyramidLoopPlan #-}

shiftablePinwheelBlobPyramidLoopPlan
  :: FFTWLock
  -> DFTPlan
  -> ShiftablePinwheelBlobPyramidParams
  -> Int
  -> [[VS.Vector (Complex Double)]]
  -> [VS.Vector (Complex Double)]
  -> VS.Vector (Complex Double)
  -> IO DFTPlan
shiftablePinwheelBlobPyramidLoopPlan lock plan (ShiftablePinwheelBlobPyramidParams _ nCenter nChannel nT nR _) n [] [] imgVec = do
  (p1, vec) <-
    idft1dGPlan
      lock
      plan
      [nCenter, nChannel, div nT (2 ^ n), div nR (2 ^ n)]
      [2, 3]
      imgVec
  return p1
shiftablePinwheelBlobPyramidLoopPlan lock plan params@(ShiftablePinwheelBlobPyramidParams _ nCenter nChannel nT nR _) n (b1':b1s) (l1':l1s) imgVec = do
  let l = VS.zipWith (*) l1' imgVec
  (p1, lVec) <-
    idft1dGPlan
      lock
      plan
      [nCenter, nChannel, div nT (2 ^ n), div nR (2 ^ n)]
      [2, 3]
      l
  let lArr =
        computeS .
        downsample [2, 2, 1, 1] .
        fromUnboxed
          (Z :. nCenter :. nChannel :. div nT (2 ^ n) :. div nR (2 ^ n)) .
        VS.convert $
        lVec
  (p2, downsampledFourierLVec) <-
    dft1dGPlan
      lock
      p1
      [nCenter, nChannel, div nT (2 ^ (n + 1)), div nR (2 ^ (n + 1))]
      [2, 3] .
    VS.convert . toUnboxed $
    lArr
  shiftablePinwheelBlobPyramidLoopPlan
    lock
    p2
    params
    (n + 1)
    b1s
    l1s
    downsampledFourierLVec
shiftablePinwheelBlobPyramidLoopPlan _ _ _ _ _ _ _ =
  error
    "shiftablePinwheelBlobPyramidLoopPlan: The lengths of l1 and h1 lists are not the same."

shiftablePinwheelBlobPyramid
  :: DFTPlan
  -> Int
  -> ShiftablePinwheelBlobPyramidFilters
  -> (Int -> ShiftablePinwheelPyramidInternalArray -> [VU.Vector Double])
  -> ShiftablePinwheelPyramidInputArray
  -> IO [[VU.Vector Double]]
shiftablePinwheelBlobPyramid plan stride' (ShiftablePinwheelBlobPyramidFilters params@(ShiftablePinwheelBlobPyramidParams _ nCenter nChannel nT nR _) h0' l0' b1' l1') f arr = do
  vec <-
    dftExecute plan (DFTPlanID DFT1DG [nCenter, nChannel, nT, nR] [2, 3]) .
    VS.convert . toUnboxed . computeS . R.map (:+ 0) $
    arr
  let h = VS.zipWith (*) h0' vec
      l = VS.zipWith (*) l0' vec
  hVec <-
    dftExecute plan (DFTPlanID IDFT1DG [nCenter, nChannel, nT, nR] [2, 3]) h
  let !hFeatures =
        f stride' .
        fromUnboxed (Z :. nCenter :. nChannel :. nT :. nR) . VS.convert $
        hVec
  (x:lFeatures) <-
    shiftablePinwheelBlobPyramidLoop plan stride' params f 0 b1' l1' l
  return $! (L.zipWith (\a b -> a VU.++ b) hFeatures x) : lFeatures

{-# INLINE shiftablePinwheelBlobPyramidLoop #-}

shiftablePinwheelBlobPyramidLoop
  :: DFTPlan
  -> Int
  -> ShiftablePinwheelBlobPyramidParams
  -> (Int -> ShiftablePinwheelPyramidInternalArray -> [VU.Vector Double])
  -> Int
  -> [[VS.Vector (Complex Double)]]
  -> [VS.Vector (Complex Double)]
  -> VS.Vector (Complex Double)
  -> IO [[VU.Vector Double]]
shiftablePinwheelBlobPyramidLoop plan stride' (ShiftablePinwheelBlobPyramidParams _ nCenter nChannel nT nR _) f n [] [] imgVec = do
  vec <-
    dftExecute
      plan
      (DFTPlanID
         IDFT1DG
         [nCenter, nChannel, div nT (2 ^ n), div nR (2 ^ n)]
         [2, 3])
      imgVec
  return
    [ f stride' .
      fromUnboxed (Z :. nCenter :. nChannel :. div nT (2 ^ n) :. div nR (2 ^ n)) .
      VS.convert $
      vec
    ]
shiftablePinwheelBlobPyramidLoop plan stride' params@(ShiftablePinwheelBlobPyramidParams _ nCenter nChannel nT nR _) f n (b1':b1s) (l1':l1s) imgVec = do
  let bs = L.map (VS.zipWith (*) imgVec) b1'
      l = VS.zipWith (*) l1' imgVec
      planID =
        DFTPlanID
          IDFT1DG
          [nCenter, nChannel, div nT (2 ^ n), div nR (2 ^ n)]
          [2, 3]
      planPtr = getDFTPlan plan planID
  hVecs <- M.mapM (dftExecuteWithPlan planID planPtr) bs
  lVec <-
    dftExecute
      plan
      (DFTPlanID
         IDFT1DG
         [nCenter, nChannel, div nT (2 ^ n), div nR (2 ^ n)]
         [2, 3])
      l
  let !hFeatures =
        L.map VU.concat .
        L.transpose .
        L.map
          (f stride' .
           fromUnboxed
             (Z :. nCenter :. nChannel :. div nT (2 ^ n) :. div nR (2 ^ n)) .
           VS.convert) $
        hVecs
  let lArr =
        computeS .
        downsample [2, 2, 1, 1] .
        fromUnboxed
          (Z :. nCenter :. nChannel :. div nT (2 ^ n) :. div nR (2 ^ n)) .
        VS.convert $
        lVec
  downsampledFourierLVec <-
    dftExecute
      plan
      (DFTPlanID
         DFT1DG
         [nCenter, nChannel, div nT (2 ^ (n + 1)), div nR (2 ^ (n + 1))]
         [2, 3]) .
    VS.convert . toUnboxed $
    lArr
  features <-
    shiftablePinwheelBlobPyramidLoop
      plan
      stride'
      params
      f
      (n + 1)
      b1s
      l1s
      downsampledFourierLVec
  return $! hFeatures : features
shiftablePinwheelBlobPyramidLoop _ _ _ _ _ _ _ _ = error "shiftablePinwheelBlobPyramidLoop: The lengths of l1 and h1 lists are not the same."


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
    newR = 0 -- div nR 4

shiftablePinwheelBlobPyramidDFT
  :: DFTPlan
  -> Int
  -> ShiftablePinwheelBlobPyramidFilters
  -> (Int -> ShiftablePinwheelPyramidInternalArray -> [VU.Vector Double])
  -> ShiftablePinwheelPyramidInputArray
  -> IO [[VU.Vector Double]]
shiftablePinwheelBlobPyramidDFT plan stride' (ShiftablePinwheelBlobPyramidFilters params@(ShiftablePinwheelBlobPyramidParams _ nCenter nChannel nT nR _) h0' l0' b1' l1') f arr = do
  vec <-
    dftExecute plan (DFTPlanID DFT1DG [nCenter, nChannel, nT, nR] [2, 3]) .
    VS.convert . toUnboxed . computeS . R.map (:+ 0) $
    arr
  let h = VS.zipWith (*) h0' vec
      l = VS.zipWith (*) l0' vec
  let !hFeatures =
        f stride' .
        fromUnboxed (Z :. nCenter :. nChannel :. nT :. nR) . VS.convert $
        h
  (x:lFeatures) <-
    shiftablePinwheelBlobPyramidLoopDFT plan stride' params f 0 b1' l1' l
  return $! (L.zipWith (\a b -> a VU.++ b) hFeatures x) : lFeatures

{-# INLINE shiftablePinwheelBlobPyramidLoopDFT #-}

shiftablePinwheelBlobPyramidLoopDFT
  :: DFTPlan
  -> Int
  -> ShiftablePinwheelBlobPyramidParams
  -> (Int -> ShiftablePinwheelPyramidInternalArray -> [VU.Vector Double])
  -> Int
  -> [[VS.Vector (Complex Double)]]
  -> [VS.Vector (Complex Double)]
  -> VS.Vector (Complex Double)
  -> IO [[VU.Vector Double]]
shiftablePinwheelBlobPyramidLoopDFT plan stride' (ShiftablePinwheelBlobPyramidParams _ nCenter nChannel nT nR _) f n [] [] imgVec =
  return
    [ f stride' .
      fromUnboxed (Z :. nCenter :. nChannel :. div nT (2 ^ n) :. div nR (2 ^ n)) .
      VS.convert $
      imgVec
    ]
shiftablePinwheelBlobPyramidLoopDFT plan stride' params@(ShiftablePinwheelBlobPyramidParams _ nCenter nChannel nT nR _) f n (b1':b1s) (l1':l1s) imgVec = do
  let bs = L.map (VS.zipWith (*) imgVec) b1'
      l = VS.zipWith (*) l1' imgVec
  lVec <-
    dftExecute
      plan
      (DFTPlanID
         IDFT1DG
         [nCenter, nChannel, div nT (2 ^ n), div nR (2 ^ n)]
         [2, 3])
      l
  let !hFeatures =
        L.map VU.concat .
        L.transpose .
        L.map
          (f stride' .
           fromUnboxed
             (Z :. nCenter :. nChannel :. div nT (2 ^ n) :. div nR (2 ^ n)) .
           VS.convert) $
        bs
  let lArr =
        computeS .
        downsample [2, 2, 1, 1] .
        fromUnboxed
          (Z :. nCenter :. nChannel :. div nT (2 ^ n) :. div nR (2 ^ n)) .
        VS.convert $
        lVec
  downsampledFourierLVec <-
    dftExecute
      plan
      (DFTPlanID
         DFT1DG
         [nCenter, nChannel, div nT (2 ^ (n + 1)), div nR (2 ^ (n + 1))]
         [2, 3]) .
    VS.convert . toUnboxed $
    lArr
  features <-
    shiftablePinwheelBlobPyramidLoopDFT
      plan
      stride'
      params
      f
      (n + 1)
      b1s
      l1s
      downsampledFourierLVec
  return $! hFeatures : features
shiftablePinwheelBlobPyramidLoopDFT _ _ _ _ _ _ _ _ = error "shiftablePinwheelBlobPyramidLoopDFT: The lengths of l1 and h1 lists are not the same."


{-# INLINE featureExtractionBlobDFT #-}

featureExtractionBlobDFT :: Int
                         -> ShiftablePinwheelPyramidInternalArray
                         -> [VU.Vector Double]
featureExtractionBlobDFT stride' arr' =
  L.map
    (\i -> toUnboxed . computeS . R.slice magArr $ (Z :. i :. All :. All :. All))
    [i | i <- [0 .. nCenter - 1]]
  where
    (Z :. nCenter :. _ :. _ :. _) = extent arr'
    magArr = R.map magnitude . downsample [stride', stride', 1, 1] $ arr'

-- shiftablePinwheel :: FFTW
--                   -> Int
--                   -> ShiftablePinwheelPyramidInputArray
--                   -> IO [VU.Vector Double]
-- shiftablePinwheel fftw downsampleFactor arr = do
--   let (Z :. numCenters :. numChannels :. nT :. nR) = extent arr
--   vec <-
--     dft1dG fftw [numCenters, numChannels, nT, nR] [2, 3] .
--     VS.convert . toUnboxed . computeS . R.map (:+ 0) $
--     arr
--   let arr1 =
--         downsample [downsampleFactor, downsampleFactor, 1, 1] .
--         -- crop [0,0,0,0] [div nR 4, div nT 4, numChannels, numCenters] .
--         fromUnboxed (Z :. numCenters :. numChannels :. nT :. nR) .
--         VS.convert . VS.map magnitude $
--         vec
--   return
--     [ l2norm . toUnboxed . computeS . R.slice arr1 $
--     (Z :. i :. All :. All :. All)
--     | i <- [0 .. numCenters - 1]
--     ]



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

-- {-# INLINE shiftablePinwheelRingLogGabor #-}

-- shiftablePinwheelRingLogGabor
--   :: FFTW
--   -> ShiftablePinwheelRingLogGaborFilters
--   -> (ShiftablePinwheelPyramidInternalArray -> [VU.Vector Double])
--   -> ShiftablePinwheelPyramidInputArray
--   -> IO [[VU.Vector Double]]
-- shiftablePinwheelRingLogGabor fftw (ShiftablePinwheelRingLogGaborFilters (ShiftablePinwheelPyramidParams _ nCenter nChannel nT nR) filters) f arr = do
--   imgVec <-
--     dft1dG fftw [nCenter, nChannel, nT, nR] [2, 3] .
--     VS.convert . toUnboxed . computeS . R.map (:+ 0) $
--     arr
--   let filteredImageFourierVecs = parMap rdeepseq (VS.zipWith (*) imgVec) filters
--   filteredImageVecs <-
--     M.mapM
--       (idft1dG fftw [nCenter, nChannel, nT, nR] [3])
--       filteredImageFourierVecs
--   -- let results =
--   --       [ L.map VU.concat .
--   --         L.transpose .
--   --         parMap
--   --           rdeepseq
--   --           (f . fromUnboxed (Z :. nCenter :. nChannel :. nT :. nR) . VS.convert) $
--   --         filteredImageVecs
--   --       ]
--   -- return results
--   return .
--     parMap
--       rdeepseq
--       (f . fromUnboxed (Z :. nCenter :. nChannel :. nT :. nR) . VS.convert) $
--     filteredImageVecs

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

{-# INLINE shiftablePinwheelBlobPyramidScatteringNetworks #-}

shiftablePinwheelBlobPyramidScatteringNetworks
  :: DFTPlan
  -> ShiftablePinwheelBlobPyramidScatteringNetworksFilters
  -> Int
  -> Int
  -> (Int -> ShiftablePinwheelPyramidInternalArray -> [VU.Vector Double])
  -> Int
  -> Int
  -> [ShiftablePinwheelPyramidInputArray]
  -> IO [[VU.Vector Double]]
shiftablePinwheelBlobPyramidScatteringNetworks plan filters 1 stride' f numLayers k arrs =
  let (Z :. numChannels :. numCenters :. nT :. nR) = extent . L.head $ arrs
  in case DFT.lookup
            (ShiftablePinwheelBlobPyramidParams
               numLayers
               numCenters
               numChannels
               nT
               nR
               k)
            filters of
       Nothing -> return []
       Just x -> do
         y <- shiftablePinwheelBlobPyramidArrayLowpass plan x stride' f arrs
         return [y]
shiftablePinwheelBlobPyramidScatteringNetworks plan filters n stride' f numLayers k arrs =
  let (Z :. numChannels :. numCenters :. nT :. nR) = extent . L.head $ arrs
  in case DFT.lookup
            (ShiftablePinwheelBlobPyramidParams
               numLayers
               numCenters
               numChannels
               nT
               nR
               k)
            filters of
       Nothing -> return []
       Just x -> do
         (bs, features) <-
           shiftablePinwheelBlobPyramidArray plan x stride' f arrs
         bsFeatures <-
           M.mapM
             (shiftablePinwheelBlobPyramidScatteringNetworks
                plan
                filters
                (n - 1)
                stride'
                f
                numLayers
                k)
             bs
         return $ features : L.concat bsFeatures


{-# INLINE shiftablePinwheelBlobPyramidArray #-}

shiftablePinwheelBlobPyramidArray
  :: DFTPlan
  -> ShiftablePinwheelBlobPyramidFilters
  -> Int
  -> (Int -> ShiftablePinwheelPyramidInternalArray -> [VU.Vector Double])
  -> [ShiftablePinwheelPyramidInputArray]
  -> IO ([[ShiftablePinwheelPyramidInputArray]], [VU.Vector Double])
shiftablePinwheelBlobPyramidArray plan (ShiftablePinwheelBlobPyramidFilters params@(ShiftablePinwheelBlobPyramidParams _ nCenter nChannel nT nR _) h0' l0' b1' l1') stride' f arr = do
  vec <-
    dftExecuteBatch plan (DFTPlanID DFT1DG [nChannel, nCenter, nT, nR] [2, 3]) .
    L.map (VS.convert . toUnboxed . computeS . R.map (:+ 0)) $
    arr
  let h = L.map (VS.zipWith (*) h0') vec
      l = L.map (VS.zipWith (*) l0') vec
  hVec <-
    dftExecuteBatch
      plan
      (DFTPlanID IDFT1DG [nChannel, nCenter, nT, nR] [2, 3])
      h
  let hFeatures =
        L.map
          (computeS .
           R.map magnitude .
           fromUnboxed (Z :. nChannel :. nCenter :. nT :. nR) . VS.convert)
          hVec
  ((x:xs), lFeatures) <-
    shiftablePinwheelBlobPyramidLoopArray plan params stride' f 0 b1' l1' l
  return $! ((hFeatures L.++ x) : xs, lFeatures)


{-# INLINE shiftablePinwheelBlobPyramidLoopArray #-}

shiftablePinwheelBlobPyramidLoopArray
  :: DFTPlan
  -> ShiftablePinwheelBlobPyramidParams
  -> Int
  -> (Int -> ShiftablePinwheelPyramidInternalArray -> [VU.Vector Double])
  -> Int
  -> [[VS.Vector (Complex Double)]]
  -> [VS.Vector (Complex Double)]
  -> [VS.Vector (Complex Double)]
  -> IO ([[ShiftablePinwheelPyramidInputArray]],[VU.Vector Double])
shiftablePinwheelBlobPyramidLoopArray plan (ShiftablePinwheelBlobPyramidParams _ nCenter nChannel nT nR _) stride' f n [] [] imgVecs = do
  -- vecs <-
  --   dftExecuteBatch
  --     plan
  --     (DFTPlanID
  --        IDFT1DG
  --        [nChannel, nCenter, div nT (2 ^ n), div nR (2 ^ n)]
  --        [2, 3])
  --     imgVecs
  return
    ( []
    , f stride' .
      fromUnboxed
        (Z :. (nChannel * L.length imgVecs) :. nCenter :. div nT (2 ^ n) :.
         div nR (2 ^ n)) .
      VS.convert . VS.concat $
      imgVecs)
  -- return
  --   ( []
  --   , f stride' .
  --     fromUnboxed
  --       (Z :. (nChannel * L.length imgVecs) :. nCenter :. div nT (2 ^ n) :.
  --        div nR (2 ^ n)) .
  --     VS.convert . VS.concat $
  --     imgVecs)
shiftablePinwheelBlobPyramidLoopArray plan params@(ShiftablePinwheelBlobPyramidParams _ nCenter nChannel nT nR _) stride' f n (b1':b1s) (l1':l1s) imgVecs = do
  let bs = L.map (\imgVec -> L.map (VS.zipWith (*) imgVec) b1') imgVecs
      l = L.map (VS.zipWith (*) l1') imgVecs
      planID =
        DFTPlanID
          IDFT1DG
          [nChannel, nCenter, div nT (2 ^ n), div nR (2 ^ n)]
          [2, 3]
      planPtr = getDFTPlan plan planID
  hVecs <- M.mapM (M.mapM (dftExecuteWithPlan planID planPtr)) bs
  lVec <- M.mapM (dftExecuteWithPlan planID planPtr) l
  let hFeatures =
        L.concatMap
          (L.map
             (computeS .
              R.map magnitude .
              fromUnboxed
                (Z :. nChannel :. nCenter :. div nT (2 ^ n) :. div nR (2 ^ n)) .
              VS.convert))
          hVecs
      lArr =
        L.map
          (computeS .
           downsample [2, 2, 1, 1] .
           fromUnboxed
             (Z :. nChannel :. nCenter :. div nT (2 ^ n) :. div nR (2 ^ n)) .
           VS.convert)
          lVec
  downsampledFourierLVec <-
    dftExecuteBatch
      plan
      (DFTPlanID
         DFT1DG
         [nChannel, nCenter, div nT (2 ^ (n + 1)), div nR (2 ^ (n + 1))]
         [2, 3]) .
    L.map (VS.convert . toUnboxed) $
    lArr
  (arrs, features) <-
    shiftablePinwheelBlobPyramidLoopArray
      plan
      params
      stride'
      f
      (n + 1)
      b1s
      l1s
      downsampledFourierLVec
  return $! (hFeatures : arrs, features)
shiftablePinwheelBlobPyramidLoopArray _ _ _ _ _ _ _ _ =
  error
    "shiftablePinwheelBlobPyramidLoopArray: The lengths of l1 and h1 lists are not the same."

{-# INLINE shiftablePinwheelBlobPyramidArrayLowpass #-}

shiftablePinwheelBlobPyramidArrayLowpass
  :: DFTPlan
  -> ShiftablePinwheelBlobPyramidFilters
  -> Int
  -> (Int -> ShiftablePinwheelPyramidInternalArray -> [VU.Vector Double])
  -> [ShiftablePinwheelPyramidInputArray]
  -> IO [VU.Vector Double]
shiftablePinwheelBlobPyramidArrayLowpass plan (ShiftablePinwheelBlobPyramidFilters params@(ShiftablePinwheelBlobPyramidParams _ nCenter nChannel nT nR _) _h0' l0' _b1' l1') stride' f arr = do
  vec <-
    dftExecuteBatch plan (DFTPlanID DFT1DG [nChannel, nCenter, nT, nR] [2, 3]) .
    L.map (VS.convert . toUnboxed . computeS . R.map (:+ 0)) $
    arr
  let l = L.map (VS.zipWith (*) l0') vec
  shiftablePinwheelBlobPyramidLoopArrayLowpass plan params stride' f 0 l1' l


{-# INLINE shiftablePinwheelBlobPyramidLoopArrayLowpass #-}

shiftablePinwheelBlobPyramidLoopArrayLowpass
  :: DFTPlan
  -> ShiftablePinwheelBlobPyramidParams
  -> Int
  -> (Int -> ShiftablePinwheelPyramidInternalArray -> [VU.Vector Double])
  -> Int
  -> [VS.Vector (Complex Double)]
  -> [VS.Vector (Complex Double)]
  -> IO [VU.Vector Double]
shiftablePinwheelBlobPyramidLoopArrayLowpass plan (ShiftablePinwheelBlobPyramidParams _ nCenter nChannel nT nR _) stride' f n [] imgVecs = do
  -- vecs <-
  --   dftExecuteBatch
  --     plan
  --     (DFTPlanID
  --        IDFT1DG
  --        [nChannel, nCenter, div nT (2 ^ n), div nR (2 ^ n)]
  --        [2, 3])
  --     imgVecs
  return .
    f stride' .
    fromUnboxed
      (Z :. (nChannel * L.length imgVecs) :. nCenter :. div nT (2 ^ n) :.
       div nR (2 ^ n)) .
    VS.convert . VS.concat $
    imgVecs
-- return .
-- f stride' .
-- fromUnboxed
--   (Z :. (nChannel * L.length imgVecs) :. nCenter :. div nT (2 ^ n) :.
--    div nR (2 ^ n)) .
-- VS.convert . VS.concat $
-- imgVecs           
shiftablePinwheelBlobPyramidLoopArrayLowpass plan params@(ShiftablePinwheelBlobPyramidParams _ nCenter nChannel nT nR _) stride' f n (l1':l1s) imgVecs = do
  let l = L.map (VS.zipWith (*) l1') imgVecs
  lVec <-
    dftExecuteBatch
      plan
      (DFTPlanID
         IDFT1DG
         [nChannel, nCenter, div nT (2 ^ n), div nR (2 ^ n)]
         [2, 3])
      l
  let lArr =
        L.map
          (computeS .
           downsample [2, 2, 1, 1] .
           fromUnboxed
             (Z :. nChannel :. nCenter :. div nT (2 ^ n) :. div nR (2 ^ n)) .
           VS.convert)
          lVec
  downsampledFourierLVec <-
    dftExecuteBatch
      plan
      (DFTPlanID
         DFT1DG
         [nChannel, nCenter, div nT (2 ^ (n + 1)), div nR (2 ^ (n + 1))]
         [2, 3]) .
    L.map (VS.convert . toUnboxed) $
    lArr
  shiftablePinwheelBlobPyramidLoopArrayLowpass
    plan
    params
    stride'
    f
    (n + 1)
    l1s
    downsampledFourierLVec

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
      computeS . fromFunction (Z :. nChannel :. nCenter :. nT'' :. nR'') $
      (\(Z :. _ :. _ :. j :. i) ->
         let a = fftwFreqOpBlob nR'' i
             b = fftwFreqOpBlob nT'' j
             r = sqrt $ a ^ (2 :: Int) + b ^ (2 :: Int)
         in op r)
    makeBandpassFilter nT' nR' opR opT =
      VS.convert .
      toUnboxed .
      computeS . fromFunction (Z :. nChannel :. nCenter :. nT' :. nR') $
      (\(Z :. _ :. _ :. tInd :. rInd) ->
         let a = fftwFreqOpBlob nR' rInd
             b = fftwFreqOpBlob nT' tInd
             r = sqrt $ a ^ (2 :: Int) + b ^ (2 :: Int)
             t = angleFunctionRad a b
         in opT t * opR r)
         

generateShiftablePinwheelBlobPyramidScatteringNetworksFilters
  :: ShiftablePinwheelBlobPyramidParams
  -> ShiftablePinwheelBlobPyramidScatteringNetworksFilters
generateShiftablePinwheelBlobPyramidScatteringNetworksFilters params@(ShiftablePinwheelBlobPyramidParams nLayer nCenter nChannel nT nR k) =
  let x =
        round . logBase 2 . fromIntegral $
        div (min (div nT (2 ^ nLayer)) (div nR (2 ^ nLayer))) 3 :: Int
  in DFT.fromList .
     L.map
       (\n ->
          let p =
                ShiftablePinwheelBlobPyramidParams
                  nLayer
                  nCenter
                  nChannel
                  (div nT (2 ^ n))
                  (div nR (2 ^ n))
                  k
          in (p, generateShiftablePinwheelBlobPyramidFilters1 p)) $
     [0 .. x]
     

{-# INLINE featureExtractionBlobScattering #-}

featureExtractionBlobScattering
  :: Int
  -> ShiftablePinwheelPyramidInternalArray
  -> [VU.Vector Double]
featureExtractionBlobScattering stride' arr' =
  L.map
    (\i -> toUnboxed . computeS . R.slice magArr $ (Z :. All :. i :. All :. All))
    [0 .. nCenter - 1]
  where
    (Z :. numChannels :. nCenter :. nT :. nR) = extent arr'
    magArr =
      R.map magnitude -- .
      -- crop [0, 0, 0, 0] [(div nR 2) + 1, (div nT 2) + 1, nCenter, numChannels] 
      $
      arr'
    

shiftablePinwheelBlobPyramidScatteringNetworksPlan :: DFTPlan
                                                   -> ShiftablePinwheelBlobPyramidScatteringNetworksFilters
                                                   -> IO DFTPlan
shiftablePinwheelBlobPyramidScatteringNetworksPlan plan filters = do
  lock <- getFFTWLock
  M.foldM
    (\plan1 (ShiftablePinwheelBlobPyramidFilters (ShiftablePinwheelBlobPyramidParams nLayer nCenter nChannel nT nR k) _ _ _ vecs) -> do
       fst <$>
         M.foldM
           (\(plan2, vec) n -> do
              (plan3, x) <-
                dft1dGPlan
                  lock
                  plan2
                  [nChannel, nCenter, div nT (2 ^ n), div nR (2 ^ n)]
                  [2, 3]
                  vec
              (plan4, y) <-
                idft1dGPlan
                  lock
                  plan3
                  [nChannel, nCenter, div nT (2 ^ n), div nR (2 ^ n)]
                  [2, 3]
                  x
              return
                ( plan4
                , VS.convert .
                  toUnboxed .
                  computeS .
                  downsample [2, 2, 1, 1] .
                  fromUnboxed
                    (Z :. nChannel :. nCenter :. div nT (2 ^ n) :.
                     div nR (2 ^ n)) .
                  VS.convert $
                  y))
           (plan1, L.head vecs)
           [0 .. L.length vecs])
    plan .
    DFT.elems $
    filters

{-# INLINE check #-}

check :: String -> VS.Vector (Complex Double) -> IO ()
check name vec =
  when
    (VS.any isNaN . VS.map magnitude $ vec)
    (putStrLn $ "something wrong with " L.++ name)
