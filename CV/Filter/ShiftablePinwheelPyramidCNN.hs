{-# LANGUAGE BangPatterns #-}
module CV.Filter.ShiftablePinwheelPyramidCNN
  ( module SPP
  , FeatureArray
  , shiftablePinwheelBlobPyramidCNN
  , shiftablePinwheelBlobPyramidScatteringNetworksCNN
  , featureArray2LabeledArrayList
  ) where

import           Control.Monad                      as M
import           CV.Array.LabeledArray
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
import Text.Printf
import CV.Utility.Utility

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

{-# INLINE shiftablePinwheelBlobPyramidArrayLowpassCNN #-}

shiftablePinwheelBlobPyramidArrayLowpassCNN
  :: DFTPlan
  -> ShiftablePinwheelBlobPyramidFilters
  -> [ShiftablePinwheelPyramidInputArray]
  -> IO [FeatureArray]
shiftablePinwheelBlobPyramidArrayLowpassCNN plan (ShiftablePinwheelBlobPyramidFilters params@(ShiftablePinwheelBlobPyramidParams _ nCenter nChannel nT nR _) _h0' l0' _b1' l1') arr = do
  vec <-
    dftExecuteBatch plan (DFTPlanID DFT1DG [nChannel, nCenter, nT, nR] [2, 3]) .
    L.map (VS.convert . toUnboxed . computeS . R.map (:+ 0)) $
    arr
  let l = L.map (VS.zipWith (*) l0') vec
  shiftablePinwheelBlobPyramidLoopArrayLowpassCNN plan params 0 l1' l


{-# INLINE shiftablePinwheelBlobPyramidLoopArrayLowpassCNN #-}

shiftablePinwheelBlobPyramidLoopArrayLowpassCNN
  :: DFTPlan
  -> ShiftablePinwheelBlobPyramidParams
  -> Int
  -> [VS.Vector (Complex Double)]
  -> [VS.Vector (Complex Double)]
  -> IO [FeatureArray]
shiftablePinwheelBlobPyramidLoopArrayLowpassCNN plan (ShiftablePinwheelBlobPyramidParams _ nCenter nChannel nT nR _) n [] imgVecs = do
  vecs <-
    dftExecuteBatch
      plan
      (DFTPlanID
         IDFT1DG
         [nChannel, nCenter, div nT (2 ^ n), div nR (2 ^ n)]
         [2, 3])
      imgVecs
  return .
    extractFeatureArray .
    fromUnboxed
      (Z :. (nChannel * L.length imgVecs) :. nCenter :. div nT (2 ^ n) :.
       div nR (2 ^ n)) .
    VS.convert . VS.concat $
    vecs
shiftablePinwheelBlobPyramidLoopArrayLowpassCNN plan params@(ShiftablePinwheelBlobPyramidParams _ nCenter nChannel nT nR _) n (l1':l1s) imgVecs = do
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
  shiftablePinwheelBlobPyramidLoopArrayLowpassCNN
    plan
    params
    (n + 1)
    l1s
    downsampledFourierLVec

{-# INLINE shiftablePinwheelBlobPyramidLoopArrayCNN #-}

shiftablePinwheelBlobPyramidLoopArrayCNN
  :: DFTPlan
  -> ShiftablePinwheelBlobPyramidParams
  -> Int
  -> [[VS.Vector (Complex Double)]]
  -> [VS.Vector (Complex Double)]
  -> [VS.Vector (Complex Double)]
  -> IO ([[ShiftablePinwheelPyramidInputArray]],[FeatureArray])
shiftablePinwheelBlobPyramidLoopArrayCNN plan (ShiftablePinwheelBlobPyramidParams _ nCenter nChannel nT nR _)  n [] [] imgVecs = do
  vecs <-
    dftExecuteBatch
      plan
      (DFTPlanID
         IDFT1DG
         [nChannel, nCenter, div nT (2 ^ n), div nR (2 ^ n)]
         [2, 3])
      imgVecs
  return
    ( []
    , extractFeatureArray .
      fromUnboxed
        (Z :. (nChannel * L.length imgVecs) :. nCenter :. div nT (2 ^ n) :.
         div nR (2 ^ n)) .
      VS.convert . VS.concat $
      vecs)
shiftablePinwheelBlobPyramidLoopArrayCNN plan params@(ShiftablePinwheelBlobPyramidParams _ nCenter nChannel nT nR _) n (b1':b1s) (l1':l1s) imgVecs = do
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
    shiftablePinwheelBlobPyramidLoopArrayCNN
      plan
      params
      (n + 1)
      b1s
      l1s
      downsampledFourierLVec
  return $! (hFeatures : arrs, features)
shiftablePinwheelBlobPyramidLoopArrayCNN _ _ _ _ _ _ =
  error
    "shiftablePinwheelBlobPyramidLoopArrayCNN: The lengths of l1 and h1 lists are not the same."

{-# INLINE shiftablePinwheelBlobPyramidArrayCNN #-}

shiftablePinwheelBlobPyramidArrayCNN
  :: DFTPlan
  -> ShiftablePinwheelBlobPyramidFilters
  -> [ShiftablePinwheelPyramidInputArray]
  -> IO ([[ShiftablePinwheelPyramidInputArray]], [FeatureArray])
shiftablePinwheelBlobPyramidArrayCNN plan (ShiftablePinwheelBlobPyramidFilters params@(ShiftablePinwheelBlobPyramidParams _ nCenter nChannel nT nR _) h0' l0' b1' l1') arr = do
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
    shiftablePinwheelBlobPyramidLoopArrayCNN plan params 0 b1' l1' l
  return $! ((hFeatures L.++ x) : xs, lFeatures)


{-# INLINE shiftablePinwheelBlobPyramidScatteringNetworksCNN #-}

shiftablePinwheelBlobPyramidScatteringNetworksCNN
  :: DFTPlan
  -> ShiftablePinwheelBlobPyramidScatteringNetworksFilters
  -> Int
  -> Int
  -> Int
  -> [ShiftablePinwheelPyramidInputArray]
  -> IO [[FeatureArray]]
shiftablePinwheelBlobPyramidScatteringNetworksCNN plan filters 1 numLayers k arrs =
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
         y <- shiftablePinwheelBlobPyramidArrayLowpassCNN plan x arrs
         return [y]
shiftablePinwheelBlobPyramidScatteringNetworksCNN plan filters n numLayers k arrs =
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
         (bs, features) <- shiftablePinwheelBlobPyramidArrayCNN plan x arrs
         bsFeatures <-
           M.mapM
             (shiftablePinwheelBlobPyramidScatteringNetworksCNN
                plan
                filters
                (n - 1)
                numLayers
                k)
             bs
         return $ features : L.concat bsFeatures


{-# INLINE featureArray2LabeledArrayList #-}

featureArray2LabeledArrayList :: Int
                              -> [[FeatureArray]]
                              -> [[LabeledArray DIM3 Double]]
featureArray2LabeledArrayList label =
  L.transpose .
  L.map
    (L.map
       (\arrList ->
          let (Z :. _ :. rows :. cols) = extent . L.head $ arrList
              nf = L.sum . L.map (L.last . listOfShape . extent) $ arrList
          in LabeledArray label .
             fromUnboxed (Z :. nf :. rows :. cols) . VU.concat . L.map (-- rescaleUnboxedVector (0,1) .
                                                                        toUnboxed) $
             arrList) .
     L.transpose . snd . L.unzip) .
  L.groupBy (\a b -> (func . fst $ a) == (func . fst $ b)) .
  L.reverse .
  L.sortOn fst .
  L.map
    (\x ->
       let (Z :. nf :. rows :. cols) = extent . L.head $ x
       in ((rows, cols, nf), x))
  where func (a,b,c) = (a,b)
