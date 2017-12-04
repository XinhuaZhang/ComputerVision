{-# LANGUAGE BangPatterns #-}
module CV.Filter.ShiftablePinwheelPyramidCNN
  ( module SPP
  , FeatureArray
  , shiftablePinwheelBlobPyramidCNN
  ) where

import           Control.Monad                      as M
import           CV.Filter.ShiftablePinwheelPyramid as SPP
import           CV.Utility.DFT                     as DFT
import           CV.Utility.Parallel
import           CV.Utility.RepaArrayUtility
import           Data.Array.Repa                    as R
import           Data.Complex
import           Data.Hashable
import           Data.List                          as L
import           Data.Vector.Storable               as VS
import           Data.Vector.Unboxed                as VU

type FeatureArray = R.Array U DIM3 Double

{-# INLINE shiftablePinwheelBlobPyramidCNNLoop #-}

shiftablePinwheelBlobPyramidCNNLoop
  :: DFTPlan
  -> ShiftablePinwheelBlobPyramidParams
  -> Int
  -> [[VS.Vector (Complex Double)]]
  -> [VS.Vector (Complex Double)]
  -> VS.Vector (Complex Double)
  -> IO [ShiftablePinwheelPyramidInternalArray]
shiftablePinwheelBlobPyramidCNNLoop plan (ShiftablePinwheelBlobPyramidParams _ nCenter nChannel nT nR _) n [] [] imgVec = do
  vec <-
    dftExecute
      plan
      (DFTPlanID
         IDFT1DG
         [nChannel, nCenter, div nT (2 ^ n), div nR (2 ^ n)]
         [2, 3])
      imgVec
  return
    [ fromUnboxed (Z :. nChannel :. nCenter :. div nT (2 ^ n) :. div nR (2 ^ n)) .
      VS.convert $
      vec
    ]
shiftablePinwheelBlobPyramidCNNLoop plan params@(ShiftablePinwheelBlobPyramidParams _ nCenter nChannel nT nR _) n (b1':b1s) (l1':l1s) imgVec = do
  let bs = L.map (VS.zipWith (*) imgVec) b1'
      l = VS.zipWith (*) l1' imgVec
  hVecs <-
    dftExecuteBatch
      plan
      (DFTPlanID
         IDFT1DG
         [nChannel, nCenter, div nT (2 ^ n), div nR (2 ^ n)]
         [2, 3])
      bs
  lVec <-
    dftExecute
      plan
      (DFTPlanID
         IDFT1DG
         [nChannel, nCenter, div nT (2 ^ n), div nR (2 ^ n)]
         [2, 3])
      l
  let !hFeatures =
        fromUnboxed
          (Z :. nChannel *
           (L.length hVecs) :. nCenter :. div nT (2 ^ n) :. div nR (2 ^ n)) .
        VS.convert . VS.concat $
        hVecs
  let lArr =
        computeS .
        downsample [2, 2, 1, 1] .
        fromUnboxed
          (Z :. nChannel :. nCenter :. div nT (2 ^ n) :. div nR (2 ^ n)) .
        VS.convert $
        lVec
  downsampledFourierLVec <-
    dftExecute
      plan
      (DFTPlanID
         DFT1DG
         [nChannel, nCenter, div nT (2 ^ (n + 1)), div nR (2 ^ (n + 1))]
         [2, 3]) .
    VS.convert . toUnboxed $
    lArr
  features <-
    shiftablePinwheelBlobPyramidCNNLoop
      plan
      params
      (n + 1)
      b1s
      l1s
      downsampledFourierLVec
  return $! hFeatures : features
shiftablePinwheelBlobPyramidCNNLoop _ _ _ _ _ _ =
  error
    "shiftablePinwheelBlobPyramidCNNLoop: The lengths of l1 and h1 lists are not the same."


{-# INLINE extractFeatureArray #-}

extractFeatureArray :: ShiftablePinwheelPyramidInternalArray -> [FeatureArray]
extractFeatureArray arr =
  let (Z :. nChannel :. nCenter :. nT :. nR) = extent arr
  in L.map
       (\i ->
          computeS . R.map magnitude . R.slice arr $
          (Z :. All :. i :. All :. All))
       [0 .. nCenter - 1]
       

shiftablePinwheelBlobPyramidCNN
  :: DFTPlan
  -> ShiftablePinwheelBlobPyramidFilters
  -> ShiftablePinwheelPyramidInputArray
  -> IO [[FeatureArray]]                      -- outtermost numCenters; innermost different sizes 
shiftablePinwheelBlobPyramidCNN plan (ShiftablePinwheelBlobPyramidFilters params@(ShiftablePinwheelBlobPyramidParams _ nCenter nChannel nT nR _) h0' l0' b1' l1') arr = do
  let imgVec = toUnboxed . computeS . R.map (:+ 0) $ arr
  vec <-
    dftExecute plan (DFTPlanID DFT1DG [nChannel, nCenter, nT, nR] [2, 3]) .
    VS.convert $
    imgVec
  let h = VS.zipWith (*) h0' vec
      l = VS.zipWith (*) l0' vec
  hVec <-
    dftExecute plan (DFTPlanID IDFT1DG [nChannel, nCenter, nT, nR] [2, 3]) h
  (x:lFeatures) <- shiftablePinwheelBlobPyramidCNNLoop plan params 0 b1' l1' l
  let (Z :. nf :. nc :. rows :. cols) = extent x
      y =
        fromUnboxed
          (Z :. nf + (nChannel * 1) :. nc :. rows :. cols)
          (-- imgVec VU.++ 
           (VS.convert hVec) VU.++ (toUnboxed x))
  return . L.transpose . L.map extractFeatureArray $ (y : lFeatures)
