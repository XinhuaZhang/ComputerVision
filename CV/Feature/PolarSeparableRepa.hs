{-# LANGUAGE BangPatterns #-}
module CV.Feature.PolarSeparableRepa where

import           Control.Monad                  as M
import           CV.Array.LabeledArray
import           CV.Filter.PolarSeparableFilter
import           CV.Utility.Parallel
import           CV.Utility.RepaArrayUtility    as RU
import           Data.Array.Repa                as R
import           Data.Complex                   as C
import           Data.Conduit
import           Data.Conduit.List              as CL
import           Data.List                      as L
import           Data.Vector.Unboxed            as VU
import Control.Monad.IO.Class


-- The following two functions are for training GMM
magnitudeFixedSizeConduit
  :: ParallelParams
  -> [[PolarSeparableFilter PolarSeparableFilterParams (R.Array U DIM2 (C.Complex Double))]]
  -> Int
  -> Conduit (R.Array U DIM3 Double) IO [VU.Vector Double]
magnitudeFixedSizeConduit parallelParams filters factor =
  awaitForever
    (\x ->
       yield .
       L.concat .
       parMapChunk parallelParams
                   rdeepseq
                   (\filter' -> multiLayerMagnitudeFixedSize filter' factor x) $
       filters)

magnitudeVariedSizeConduit
  :: ParallelParams
  -> [[PolarSeparableFilterParams]]
  -> Int
  -> Conduit (R.Array U DIM3 Double) IO [VU.Vector Double]
magnitudeVariedSizeConduit parallelParams filterParamsList factor =
  awaitForever
    (\x ->
       yield .
       L.concat .
       parMapChunk
         parallelParams
         rdeepseq
         (\filterParams -> multiLayerMagnitudeVariedSize filterParams factor x) $
       filterParamsList)

-- The following two functions are for computing Fisher kernels
labeledArrayMagnitudeSetFixedSizeConduit
  :: ParallelParams
  -> [PolarSeparableFilter PolarSeparableFilterParamsSet (R.Array U DIM3 (C.Complex Double))]
  -> Int
  -> Conduit (LabeledArray DIM3 Double) IO (Int,[VU.Vector Double])
labeledArrayMagnitudeSetFixedSizeConduit parallelParams filters factor = do
  xs <- CL.take (batchSize parallelParams)
  unless
    (L.null xs)
    (do let ys =
              parMapChunk
                parallelParams
                rdeepseq
                (\(LabeledArray label x) ->
                    (label, multiLayerMagnitudeSetFixedSize filters factor x))
                xs
        sourceList ys
        labeledArrayMagnitudeSetFixedSizeConduit parallelParams filters factor)


labeledArrayMagnitudeSetVariedSizeConduit
  :: ParallelParams
  -> [PolarSeparableFilterParamsSet]
  -> Int
  -> Conduit (LabeledArray DIM3 Double) IO (Int,[VU.Vector Double])
labeledArrayMagnitudeSetVariedSizeConduit parallelParams filterParamsList factor = do
  xs <- CL.take (batchSize parallelParams)
  unless
    (L.null xs)
    (do let ys =
              parMapChunk
                parallelParams
                rdeepseq
                (\(LabeledArray label x) ->
                    ( label
                    , multiLayerMagnitudeSetVariedSize filterParamsList factor x))
                xs
        liftIO $ (\(LabeledArray label x) -> print $ extent x) $ L.head xs        
        sourceList ys
        labeledArrayMagnitudeSetVariedSizeConduit
          parallelParams
          filterParamsList
          factor)

-- The following twos functions give results of the last layer
{-# INLINE multiLayerMagnitudeFixedSize #-}

multiLayerMagnitudeFixedSize
  :: [PolarSeparableFilter PolarSeparableFilterParams (R.Array U DIM2 (C.Complex Double))]
  -> Int
  -> R.Array U DIM3 Double
  -> [VU.Vector Double]
multiLayerMagnitudeFixedSize filters facotr img =
  let !arr =
        L.foldl'
          (\arr' filter' -> computeUnboxedS . applyFilterFixedSize filter' $ arr')
          (computeUnboxedS . R.map (:+ 0) $ img)
          filters
      !(Z :. nf :. _ :. _) = extent arr
      !downSampledArr = RU.downsample [facotr, facotr, 1] arr
      !magnitudeArr = R.map C.magnitude downSampledArr
  in [ toUnboxed . computeUnboxedS . R.slice magnitudeArr $
      (Z :. k :. All :. All)
     | k <- [0 .. nf - 1] ]


-- The following twos functions give a concatnation of results from layer 1 to layer n.
{-# INLINE multiLayerMagnitudeSetFixedSize #-}

multiLayerMagnitudeSetFixedSize
  :: [PolarSeparableFilter PolarSeparableFilterParamsSet (R.Array U DIM3 (C.Complex Double))]
  -> Int
  -> R.Array U DIM3 Double
  -> [VU.Vector Double]
multiLayerMagnitudeSetFixedSize filters facotr img =
  L.concatMap
    (\arr ->
        let !(Z :. nf :. _ :. _) = extent arr
            !downSampledArr = RU.downsample [facotr, facotr, 1] arr
            !magnitudeArr = R.map C.magnitude downSampledArr
        in [ toUnboxed . computeUnboxedS . R.slice magnitudeArr $
            (Z :. k :. All :. All)
           | k <- [0 .. nf - 1] ]) .
  L.tail .
  L.scanl'
    (\arr filter' -> computeUnboxedS . applyFilterSetFixedSize filter' $ arr)
    (computeUnboxedS . R.map (:+ 0) $ img) $
  filters



{-# INLINE multiLayerMagnitudeVariedSize #-}

multiLayerMagnitudeVariedSize
  :: [PolarSeparableFilterParams]
  -> Int
  -> R.Array U DIM3 Double
  -> [VU.Vector Double]
multiLayerMagnitudeVariedSize filterParamsList facotr img =
  let !arr =
        L.foldl'
          (\arr' filterParams ->
              computeUnboxedS .
              R.map C.magnitude .
              applyFilterVariedSize filterParams . R.map (:+ 0) $
              arr')
          img
          filterParamsList
      !(Z :. nf :. _ :. _) = extent arr
      !downSampledArr = RU.downsample [facotr, facotr, 1] arr
  in [ toUnboxed . computeUnboxedS . R.slice downSampledArr $
      (Z :. k :. All :. All)
     | k <- [0 .. nf - 1] ]


{-# INLINE multiLayerMagnitudeSetVariedSize #-}

multiLayerMagnitudeSetVariedSize
  :: [PolarSeparableFilterParamsSet]
  -> Int
  -> R.Array U DIM3 Double
  -> [VU.Vector Double]
multiLayerMagnitudeSetVariedSize filterParamsList facotr img =
  L.concatMap
    (\arr ->
        let !(Z :. nf :. _ :. _) = extent arr
            !downSampledArr = RU.downsample [facotr, facotr, 1] arr
            !magnitudeArr = R.map C.magnitude downSampledArr
        in [ toUnboxed . computeUnboxedS . R.slice magnitudeArr $
            (Z :. k :. All :. All)
           | k <- [0 .. nf - 1] ]) .
  L.tail .
  L.scanl'
    (\arr filterParams ->
        computeUnboxedS . applyFilterSetVariedSize filterParams $ arr)
    (computeUnboxedS . R.map (:+ 0) $ img) $
  filterParamsList
