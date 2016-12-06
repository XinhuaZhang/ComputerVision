{-# LANGUAGE BangPatterns #-}
module CV.Feature.PolarSeparableRepa where

import           Control.Monad                  as M
import           CV.Array.LabeledArray
import           CV.Filter.PolarSeparableFilter
import           CV.Utility.Parallel
import           CV.Utility.RepaArrayUtility    as RU
import           Data.Array.CArray              as CA
import           Data.Array.Repa                as R
import           Data.Complex                   as C
import           Data.Conduit
import           Data.Conduit.List              as CL
import           Data.List                      as L
import           Data.Vector.Unboxed            as VU

magnitudeFixedSizeConduit
  :: ParallelParams
  -> [[PolarSeparableFilter (CArray (Int, Int) (C.Complex Double))]]
  -> Int
  -> Conduit (R.Array U DIM3 Double) IO [VU.Vector Double]
magnitudeFixedSizeConduit parallelParams filters factor = do
  xs <- CL.take (batchSize parallelParams)
  unless
    (L.null xs)
    (do let !ys =
              parMapChunk
                parallelParams
                rdeepseq
                (\x ->
                    L.concatMap
                      (\filter' -> multiLayerMagnitudeFixedSize filter' factor x)
                      filters)
                xs
        sourceList ys
        magnitudeFixedSizeConduit parallelParams filters factor)

magnitudeVariedSizeConduit
  :: ParallelParams
  -> [[PolarSeparableFilterParams]]
  -> Int
  -> Conduit (R.Array U DIM3 Double) IO [VU.Vector Double]
magnitudeVariedSizeConduit parallelParams filterParamsList factor = do
  xs <- CL.take (batchSize parallelParams)
  unless
    (L.null xs)
    (do let !ys =
              parMapChunk
                parallelParams
                rdeepseq
                (\x ->
                    L.concatMap
                      (\filterParams ->
                          multiLayerMagnitudeVariedSize filterParams factor x)
                      filterParamsList)
                xs
        sourceList ys
        magnitudeVariedSizeConduit parallelParams filterParamsList factor)


labeledArrayMagnitudeFixedSizeConduit
  :: ParallelParams
  -> [[PolarSeparableFilter (CArray (Int, Int) (C.Complex Double))]]
  -> Int
  -> Conduit (LabeledArray DIM3 Double) IO (Int,[VU.Vector Double])
labeledArrayMagnitudeFixedSizeConduit parallelParams filters factor = do
  xs <- CL.take (batchSize parallelParams)
  unless
    (L.null xs)
    (do let !ys =
              parMapChunk
                parallelParams
                rdeepseq
                (\(LabeledArray label x) ->
                    ( label
                    , L.concatMap
                        (\filter' ->
                            multiLayerMagnitudeFixedSize filter' factor x)
                        filters))
                xs
        sourceList ys
        labeledArrayMagnitudeFixedSizeConduit parallelParams filters factor)


labeledArrayMagnitudeVariedSizeConduit
  :: ParallelParams
  -> [[PolarSeparableFilterParams]]
  -> Int
  -> Conduit (LabeledArray DIM3 Double) IO (Int,[VU.Vector Double])
labeledArrayMagnitudeVariedSizeConduit parallelParams filterParamsList factor = do
  xs <- CL.take (batchSize parallelParams)
  unless
    (L.null xs)
    (do let !ys =
              parMapChunk
                parallelParams
                rdeepseq
                (\(LabeledArray label x) ->
                    ( label
                    , L.concatMap
                        (\filterParams ->
                            multiLayerMagnitudeVariedSize filterParams factor x)
                        filterParamsList))
                xs
        sourceList ys
        labeledArrayMagnitudeVariedSizeConduit parallelParams filterParamsList factor)

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
            !downSampledArr = RU.downsample [facotr, facotr, 1] arr
            !magnitudeArr = R.map C.magnitude downSampledArr
        in [ toUnboxed . computeUnboxedS . R.slice magnitudeArr $
            (Z :. k :. All :. All)
           | k <- [0 .. nf - 1] ]) .
  L.tail .
  L.scanl'
    (\arr filterParams ->
        computeUnboxedS . applyFilterVariedSize filterParams $ arr)
    (computeUnboxedS . R.map (:+ 0) $ img) $
  filterParamsList
