{-# LANGUAGE FlexibleContexts #-}
module Application.RecenterImage.Conduit where

import           Control.Monad                as M
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           CV.Array.LabeledArray
import           CV.Filter.GaussianFilter     as Gaussian
import           CV.Utility.Parallel
import           Data.Array.Repa              as R
import           Data.Complex
import           Data.Conduit
import           Data.Conduit.List            as CL
import           Data.Ix
import           Data.List                    as L
import           Data.Maybe
import           Data.Ord
import           Data.Vector.Unboxed          as VU
import CV.V4Filter

{-# INLINE findCenter #-}

findCenter
  :: (R.Source s Double)
  => R.Array s DIM3 Double -> (Int, Int)
findCenter arr =
  fst .
  VU.maximumBy (comparing snd) .
  VU.zip (VU.fromList . range $ ((0, 0), (ny' - 1, nx' - 1))) .
  L.foldl1' (VU.zipWith (+)) .
  L.map (\i -> toUnboxed . computeS . R.slice arr $ (Z :. i :. All :. All)) $
  [0 .. nf' - 1]
  where
    (Z :. nf' :. ny' :. nx') = extent arr


recenterFixedSizeConduit
  :: ParallelParams
  -> GaussianFilter (R.Array U DIM2 (Complex Double))
  -> Conduit (LabeledArray DIM3 Double) (ResourceT IO) (LabeledArray DIM3 Double)
recenterFixedSizeConduit parallelParams gaussianFilter = do
  xs <- CL.take (batchSize parallelParams)
  unless
    (L.null xs)
    (do let ys =
              parMapChunk
                parallelParams
                rdeepseq
                (\(LabeledArray label x) ->
                   let (yIdx, xIdx) =
                         findCenter . Gaussian.applyFilterFixedSize gaussianFilter $ x
                       (Z :. nf' :. ny' :. nx') = extent x
                       arr =
                         computeUnboxedS $
                         backpermuteDft
                           (fromFunction
                              (Z :. nf' :. ny' * 2 :. nx' * 2)
                              (\_ -> 0))
                           (\(Z :. k :. j :. i) ->
                              let j' = j - (ny' - yIdx)
                                  i' = i - (nx' - xIdx)
                              in if (j' < 0 || (j' >= ny')) ||
                                    (i' < 0 || (i' >= nx'))
                                   then Nothing
                                   else Just (Z :. k :. j' :. i'))
                           x
                   in LabeledArray label arr)
                xs
        sourceList ys
        recenterFixedSizeConduit parallelParams gaussianFilter)


findCenterConduit
  :: (R.Source s Double)
  => ParallelParams
  -> Conduit (R.Array s DIM3 Double) (ResourceT IO) (R.Array U DIM3 Double)
findCenterConduit parallelParams = do
  xs <- CL.take (batchSize parallelParams)
  unless
    (L.null xs)
    (do let ys =
              parMapChunk
                parallelParams
                rseq
                (\x ->
                   let gaussianParams = GaussianFilterParams 32 ny' nx'
                       filteredImage = Gaussian.applyFilterVariedSize gaussianParams $ x
                       (yIdx, xIdx) = findCenter filteredImage
                       (Z :. nf' :. ny' :. nx') = extent x
                       arr =
                         computeUnboxedS $
                         R.traverse
                           x
                           id
                           (\f idx@(Z :. k :. j :. i) ->
                              if i == xIdx || j == yIdx
                                then 255
                                else f idx)
                   in deepSeqArray arr ((yIdx, xIdx), arr))
                xs
        liftIO . print . fst . L.unzip $ ys
        sourceList . snd . L.unzip $ ys
        findCenterConduit parallelParams)


applyV4SeparableFilterLabeledArrayWithCenterConduit
  :: ParallelParams
  -> GaussianFilter (R.Array U DIM2 (Complex Double))
  -> V4SeparableFilterParamsAxis
  -> Conduit (LabeledArray DIM3 Double) (ResourceT IO) (Double,VU.Vector Double)
applyV4SeparableFilterLabeledArrayWithCenterConduit parallelParams gaussianFilter filterParams = do
  xs <- CL.take (batchSize parallelParams)
  unless
    (L.null xs)
    (do let ys =
              parMapChunk
                parallelParams
                rdeepseq
                (\(LabeledArray label' x) ->
                   let (Z :. channels :. numRows :. numCols) = extent x
                       imgVecs =
                         L.map
                           (\i ->
                              VU.map (:+ 0) . toUnboxed . computeS . R.slice x $
                              (Z :. i :. All :. All))
                           [0 .. channels - 1]
                       center =
                         findCenter .
                         Gaussian.applyFilterFixedSize gaussianFilter $
                         x
                       filters =
                         generateV4SeparableFilterWithCenterGrid
                           filterParams
                           center
                   in ( fromIntegral label'
                      , normalizeVec .
                        VU.concat .
                        L.map
                          (\filter' ->
                             VU.concat $
                             L.map (applyV4SeparableFilter filter') imgVecs) $
                        filters))
                xs
        sourceList ys
        applyV4SeparableFilterLabeledArrayWithCenterConduit
          parallelParams
          gaussianFilter
          filterParams)
          


applyV4SeparableFilterComplexLabeledArrayWithCenterConduit
  :: ParallelParams
  -> GaussianFilter (R.Array U DIM2 (Complex Double))
  -> V4SeparableFilterParamsAxis
  -> Conduit (LabeledArray DIM3 Double) (ResourceT IO) (Double,VU.Vector (Complex Double))
applyV4SeparableFilterComplexLabeledArrayWithCenterConduit parallelParams gaussianFilter filterParams = do
  xs <- CL.take (batchSize parallelParams)
  unless
    (L.null xs)
    (do let ys =
              parMapChunk
                parallelParams
                rdeepseq
                (\(LabeledArray label' x) ->
                    let (Z :. channels :. numRows :. numCols) = extent x
                        imgVecs =
                          L.map
                            (\i ->
                                VU.map (:+ 0) . toUnboxed . computeS . R.slice x $
                                (Z :. i :. All :. All))
                            [0 .. channels - 1]
                        center =
                          findCenter . Gaussian.applyFilterFixedSize gaussianFilter $
                          x
                        filters =
                          generateV4SeparableFilterWithCenterGrid filterParams center
                        complexVec =
                          VU.concat .
                          L.map
                            (\filter' ->
                               VU.concat $
                               L.map
                                 (applyV4SeparableFilterComplex filter')
                                 imgVecs) $
                          filters
                        normalizedMagVec =
                          normalizeVec . VU.map magnitude $ complexVec
                    in ( fromIntegral label'
                       , VU.zipWith (\a b -> mkPolar a . phase $ b) normalizedMagVec complexVec))
                xs
        sourceList ys
        applyV4SeparableFilterComplexLabeledArrayWithCenterConduit
          parallelParams
          gaussianFilter
          filterParams)


{-# INLINE normalizeVec #-}

normalizeVec :: VU.Vector Double -> VU.Vector Double
normalizeVec vec
  | s == 0 = VU.replicate (VU.length vec) 0
  | otherwise = VU.map (/ s) vec
  where
    s = sqrt . VU.sum . VU.map (^ 2) $ vec
