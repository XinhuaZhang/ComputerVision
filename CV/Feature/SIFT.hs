{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes      #-}

module CV.Feature.SIFT where

import           Control.Monad                as M
import           Control.Monad.Trans.Resource
import           CV.Array.LabeledArray
import           CV.Filter.GaussianFilter
import           CV.Utility.Parallel
import           CV.Utility.RepaArrayUtility
import           Data.Array.Repa              as R
import           Data.Array.Repa.Stencil      as R
import           Data.Array.Repa.Stencil.Dim2 as R
import           Data.Complex
import           Data.Conduit
import           Data.Conduit.List            as CL
import           Data.List                    as L
import           Data.Vector.Unboxed          as VU

data SIFTParams = SIFTParams
  { scaleSIFT  :: ![Double]
  , strideSIFT :: !Int
  } deriving (Show)

getGradient
  :: (R.Source s Double)
  => Array s DIM2 Double -> (Array U DIM2 Double, Array U DIM2 Double)
getGradient arr =
  -- deepSeqArrays [magnitude', orientation] 
  (magnitude', orientation)
  where
    xStencil =
      [stencil2| 0 0 0
                -1 0 1
                0 0 0 |]
    yStencil =
      [stencil2| 0 -1 0
                 0 0 0
                 0 1 0 |]
    !xArr = mapStencil2 BoundClamp xStencil arr
    !yArr = mapStencil2 BoundClamp yStencil arr
    !magnitude' =
      computeS . traverse2 xArr yArr const $
      \fx fy idx -> sqrt $ fx idx ^ (2 :: Int) + fy idx ^ (2 :: Int)
    !orientation =
      computeS . traverse2 xArr yArr const $
      \fx fy idx ->
         if fx idx == 0 && fy idx == 0
           then 0
           else atan $ fy idx / fx idx

{-# INLINE gaussianWindow #-}

gaussianWindow
  :: (R.Source s Double)
  => Array s DIM2 Double -> Array D DIM2 Double
gaussianWindow arr = arr *^ weight
  where
    !(Z :. nRows :. nCols) = extent arr
    !rowCenter = (fromIntegral nRows - 1) / 2
    !colCenter = (fromIntegral nCols - 1) / 2
    !s = 0.5 * fromIntegral (max nRows nCols)
    !weight =
      fromFunction (extent arr) $
      \(Z :. j :. i) ->
         gaussian2DDouble
           s
           (fromIntegral j - rowCenter)
           (fromIntegral i - colCenter)

-- 16x16 region
{-# INLINE getDescriptor #-}

getDescriptor
  :: (R.Source s Double)
  => (Array s DIM2 Double, Array s DIM2 Double) -> VU.Vector Double
getDescriptor (mag, ori) =
  normalizeVec .
  VU.map
    (\x ->
        if x > 0.2
          then 0.2
          else x) .
  normalizeVec $
  vec
  where
    !weightedMag = gaussianWindow mag
    !startPoint = [0, 4, 8, 12]
    !startPointList =
      [ [x, y]
      | x <- startPoint
      , y <- startPoint ]
    !vec =
      VU.concat .
      L.map
        (\start ->
            let !croppedMag = computeS $ crop start [4, 4] weightedMag
                !croppedOri = computeS $ crop start [4, 4] ori
            in orientationHist $
               VU.zip (toUnboxed croppedMag) (toUnboxed croppedOri)) $
      startPointList
    normalizeVec v =
      let !norm = sqrt . VU.sum . VU.map (^ (2 :: Int)) $ v
      in if norm == 0
           then v
           else VU.map (/ norm) v

{-# INLINE orientationHist #-}

orientationHist :: VU.Vector (Double, Double) -> VU.Vector Double

orientationHist =
  VU.accumulate (+) (VU.replicate 8 0) .
  VU.map
    (\(mag, ori) ->
        let !normalizedOri =
              if ori < 0 || ori >= (2 * pi)
                then if ori < 0
                       then ori + 2 * pi
                       else ori - 2 * pi
                else ori
            !idx = floor (4 * normalizedOri / pi)
            !idx1 =
              if idx >= 8
                then 7
                else idx
        in (idx1, mag))

getSIFTFeatureFromGradient
  :: (R.Source s Double)
  => Int -> (Array s DIM2 Double, Array s DIM2 Double) -> [VU.Vector Double]
getSIFTFeatureFromGradient stride (mag, ori) =
  L.map
    (\start ->
        let !croppedMag = crop start [patchSize, patchSize] mag
            !croppedOri = crop start [patchSize, patchSize] ori
        in getDescriptor (croppedMag, croppedOri))
    startPairList
  where
    !(Z :. nRows :. nCols) = extent mag
    !patchSize = 16
    !startPairList =
      [ [x, y]
      | x <- computeStartPointList nCols
      , y <- computeStartPointList nRows ]
    computeStartPointList x =
      let !num = div x stride
          !margin = div (x - stride * num) 2
      in L.filter (\y -> y + patchSize <= x) $
         L.take num [margin,margin + stride .. x]
         

getSIFTFeaturePointFromGradient
  :: (R.Source s Double)
  => (Int, Int) -> (Array s DIM2 Double, Array s DIM2 Double) -> VU.Vector Double
getSIFTFeaturePointFromGradient (j, i) (mag, ori) =
  getDescriptor (croppedMag, croppedOri)
  where
    patchSize = 16
    y = j - div patchSize 2
    x = i - div patchSize 2
    croppedMag = crop [y, x] [patchSize, patchSize] mag
    croppedOri = crop [y, x] [patchSize, patchSize] ori


-- The vectors are point-wise vector
siftFixedSizeConduit
  :: ParallelParams
  -> SIFTParams
  -> [GaussianFilter (R.Array U DIM2 (Complex Double))]
  -> Conduit (LabeledArray DIM3 Double) (ResourceT IO) [VU.Vector Double]
siftFixedSizeConduit parallelParams sp@(SIFTParams _ stride) filters' = do
  xs <- CL.take (batchSize parallelParams)
  unless
    (L.null xs)
    (do let !ys =
              parMapChunk
                parallelParams
                rdeepseq
                (\(LabeledArray _ arr) ->
                    let !(Z :. nf' :. _ :. _) = extent arr
                        !filterArrs = L.map (`applyFilterFixedSize` arr) filters'
                    in L.map VU.concat .
                       L.transpose .
                       L.map
                         (\channelIdx ->
                             L.map VU.concat .
                             L.transpose .
                             L.map
                               (\filterArr ->
                                   getSIFTFeatureFromGradient stride . getGradient $
                                   R.slice
                                     filterArr
                                     (Z :. channelIdx :. All :. All)) $
                             filterArrs) $
                       [0 .. nf' - 1])
                xs
        sourceList ys
        siftFixedSizeConduit parallelParams sp filters')
        
siftVariedSizeConduit
  :: ParallelParams
  -> SIFTParams
  -> [GaussianFilterParams]
  -> Conduit (LabeledArray DIM3 Double) (ResourceT IO) [VU.Vector Double]
siftVariedSizeConduit parallelParams sp@(SIFTParams _ stride) filterParams = do
  xs <- CL.take (batchSize parallelParams)
  unless
    (L.null xs)
    (do let !ys =
              parMapChunk
                parallelParams
                rdeepseq
                (\(LabeledArray _ arr) ->
                    let !(Z :. nf' :. _ :. _) = extent arr
                        !filterArrs = L.map (`applyFilterVariedSize` arr) filterParams
                    in L.map VU.concat .
                       L.transpose .
                       L.map
                         (\i ->
                             L.map VU.concat .
                             L.transpose .
                             L.map
                               (\filterArr ->
                                   getSIFTFeatureFromGradient stride . getGradient $
                                   R.slice filterArr (Z :. i :. All :. All)) $
                             filterArrs) $
                       [0 .. nf' - 1])
                xs
        sourceList ys
        siftVariedSizeConduit parallelParams sp filterParams)


labeledArraySIFTFixedSizeConduit
  :: ParallelParams
  -> SIFTParams
  -> [GaussianFilter (R.Array U DIM2 (Complex Double))]
  -> Conduit (LabeledArray DIM3 Double) (ResourceT IO) (Int,[VU.Vector Double])
labeledArraySIFTFixedSizeConduit parallelParams sp@(SIFTParams _ stride) filters' = do
  xs <- CL.take (batchSize parallelParams)
  unless
    (L.null xs)
    (do let !ys =
              parMapChunk
                parallelParams
                rdeepseq
                (\(LabeledArray label arr) ->
                    let !(Z :. nf' :. _ :. _) = extent arr
                        !filterArrs = L.map (`applyFilterFixedSize` arr) filters'
                    in ( label
                       , L.map VU.concat .
                         L.transpose .
                         L.map
                           (\channelIdx ->
                               L.map VU.concat .
                               L.transpose .
                               L.map
                                 (\filterArr ->
                                     getSIFTFeatureFromGradient stride . getGradient $
                                     R.slice
                                       filterArr
                                       (Z :. channelIdx :. All :. All)) $
                               filterArrs) $
                         [0 .. nf' - 1]))
                xs
        sourceList ys
        labeledArraySIFTFixedSizeConduit parallelParams sp filters')

labeledArraySIFTVariedSizeConduit
  :: ParallelParams
  -> SIFTParams
  -> [GaussianFilterParams]
  -> Conduit (LabeledArray DIM3 Double) (ResourceT IO) (Int, [VU.Vector Double])
labeledArraySIFTVariedSizeConduit parallelParams sp@(SIFTParams _ stride) filterParams = do
  xs <- CL.take (batchSize parallelParams)
  unless
    (L.null xs)
    (do let !ys =
              parMapChunk
                parallelParams
                rdeepseq
                (\(LabeledArray label arr) ->
                    let !(Z :. nf' :. _ :. _) = extent arr
                        !filterArrs = L.map (`applyFilterVariedSize` arr) filterParams
                    in ( label
                       , L.map VU.concat .
                         L.transpose .
                         L.map
                           (\i ->
                               L.map VU.concat .
                               L.transpose .
                               L.map
                                 (\filterArr ->
                                     getSIFTFeatureFromGradient stride . getGradient $
                                     R.slice filterArr (Z :. i :. All :. All)) $
                               filterArrs) $
                         [0 .. nf' - 1]))
                xs
        sourceList ys
        labeledArraySIFTVariedSizeConduit parallelParams sp filterParams)
