{-# LANGUAGE BangPatterns #-}
module CV.Feature.PolarSeparableRepa where

import           CV.Array.LabeledArray
import           CV.Filter.PolarSeparableFilter
import           CV.Utility.Parallel
import           CV.Utility.RepaArrayUtility    as RU
import           Data.Array.CArray              as CA
import           Data.Array.Repa                as R
import           Data.Complex                   as C
import           Data.Conduit
import           Data.List                      as L
import           Data.Vector.Unboxed            as VU

magnitudeFixedSizeConduit
  :: ParallelParams
  -> [[PolarSeparableFilter (CArray (Int, Int) (C.Complex Double))]]
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


labeledArrayMagnitudeFixedSizeConduit
  :: ParallelParams
  -> [[PolarSeparableFilter (CArray (Int, Int) (C.Complex Double))]]
  -> Int
  -> Conduit (LabeledArray DIM3 Double) IO (Int,[VU.Vector Double])
labeledArrayMagnitudeFixedSizeConduit parallelParams filters factor =
  awaitForever
    (yield .
     (\(LabeledArray label x) ->
        (label
        ,L.concat .
         parMapChunk
           parallelParams
           rdeepseq
           (\filterParams -> multiLayerMagnitudeFixedSize filterParams factor x) $
         filters)))



labeledArrayMagnitudeVariedSizeConduit
  :: ParallelParams
  -> [[PolarSeparableFilterParams]]
  -> Int
  -> Conduit (LabeledArray DIM3 Double) IO (Int,[VU.Vector Double])
labeledArrayMagnitudeVariedSizeConduit parallelParams filterParamsList factor =
  awaitForever
    (yield .
     (\(LabeledArray label x) ->
        (label
        ,L.concat .
         parMapChunk
           parallelParams
           rdeepseq
           (\filterParams ->
              multiLayerMagnitudeVariedSize filterParams factor x) $
         filterParamsList)))

{-# INLINE multiLayerMagnitudeFixedSize #-}

multiLayerMagnitudeFixedSize
  :: [PolarSeparableFilter (CArray (Int, Int) (C.Complex Double))]
  -> Int
  -> R.Array U DIM3 Double
  -> [VU.Vector Double]
multiLayerMagnitudeFixedSize filters facotr img =
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
    (\arr filter' -> computeUnboxedS . applyFilterFixedSize filter' $ arr)
    (computeUnboxedS . R.map (:+ 0) $ img) $
  filters


{-# INLINE multiLayerMagnitudeVariedSize #-}

multiLayerMagnitudeVariedSize
  :: [PolarSeparableFilterParams]
  -> Int
  -> R.Array U DIM3 Double
  -> [VU.Vector Double]
multiLayerMagnitudeVariedSize filterParamsList facotr img =
  L.concatMap
    (\arr ->
       let !(Z :. nf :. _ :. _) = extent arr
           !downSampledArr =
             RU.downsample [facotr,facotr,1]
                           arr
       in [toUnboxed . computeUnboxedS . R.slice downSampledArr $
           (Z :. k :. All :. All)|k <- [0 .. nf - 1]]) .
  L.tail .
  L.scanl' (\arr filterParams ->
              computeUnboxedS .
              R.map C.magnitude .
              applyFilterVariedSize filterParams . R.map (:+ 0) $
              arr)
           img $
  filterParamsList
