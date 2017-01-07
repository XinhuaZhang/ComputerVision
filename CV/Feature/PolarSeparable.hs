{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}

module CV.Feature.PolarSeparable where

import           Control.DeepSeq
import           Control.Monad                  as M
import           Control.Monad.Trans.Resource
import           CV.Array.LabeledArray
import           CV.Filter.PolarSeparableFilter
import           CV.Utility.Parallel
import           CV.Utility.RepaArrayUtility    as RU
import           Data.Array.Repa                as R
import           Data.Binary
import           Data.Complex                   as C
import           Data.Conduit
import           Data.Conduit.List              as CL
import           Data.List                      as L
import           Data.Vector.Unboxed            as VU
import           GHC.Generics
import           Prelude                        as P

data PolarSeparableFeaturePoint a = PolarSeparableFeaturePoint
  { x       :: !Int
  , y       :: !Int
  , feature :: a
  } deriving (Show, Read, Generic)

instance NFData (PolarSeparableFeaturePoint a) where
  rnf !_ = ()

instance (Binary a) =>
         Binary (PolarSeparableFeaturePoint a) where
  put (PolarSeparableFeaturePoint x' y' feature') = do
    put x'
    put y'
    put feature'
  get = do
    x' <- get
    y' <- get
    feature' <- get
    return (PolarSeparableFeaturePoint x' y' feature')

instance Functor PolarSeparableFeaturePoint where
  fmap f (PolarSeparableFeaturePoint x' y' feature') =
    PolarSeparableFeaturePoint x' y' (f feature')
    

singleLayerMagnitudeFixedSizedConduit
  :: (R.Source s Double)
  => ParallelParams
  -> PolarSeparableFilter PolarSeparableFilterParamsSet (Array U DIM3 (C.Complex Double))
  -> Int -> Conduit (Array s DIM3 Double) (ResourceT IO) (Array U DIM3 Double)
singleLayerMagnitudeFixedSizedConduit parallelParams filter' factor = do
  xs <- CL.take (batchSize parallelParams)
  unless
    (L.null xs)
    (do let !ys =
              parMapChunk
                parallelParams
                rseq
                (\x' ->
                    let !y' =
                          computeUnboxedS .
                          singleLayerMagnitudeFixedSize filter' factor $
                          x'
                    in deepSeqArray y' y')
                xs
        sourceList ys
        singleLayerMagnitudeFixedSizedConduit parallelParams filter' factor)

singleLayerMagnitudeVariedSizedConduit
  :: (R.Source s Double)
  => ParallelParams
  -> PolarSeparableFilterParamsSet
  -> Int -> Conduit (Array s DIM3 Double) (ResourceT IO) (Array U DIM3 Double)
singleLayerMagnitudeVariedSizedConduit parallelParams filterParams factor = do
  xs <- CL.take (batchSize parallelParams)
  unless
    (L.null xs)
    (do let !ys =
              parMapChunk
                parallelParams
                rseq
                (\x' ->
                    let !y' =
                          computeUnboxedS .
                          singleLayerMagnitudeVariedSize filterParams factor $
                          x'
                    in deepSeqArray y' y')
                xs
        sourceList ys
        singleLayerMagnitudeVariedSizedConduit parallelParams filterParams factor)

multiLayerMagnitudeFixedSizedConduit
  :: (R.Source s Double)
  => ParallelParams
  -> [PolarSeparableFilter PolarSeparableFilterParamsSet (Array U DIM3 (C.Complex Double))]
  -> Int -> Conduit (Array s DIM3 Double) (ResourceT IO) [[VU.Vector Double]]
multiLayerMagnitudeFixedSizedConduit parallelParams filters' factor = do
  xs <- CL.take (batchSize parallelParams)
  unless
    (L.null xs)
    (do let !ys =
              parMapChunk
                parallelParams
                rdeepseq
                (multiLayerMagnitudeFixedSize filters' factor)
                xs
        sourceList ys
        multiLayerMagnitudeFixedSizedConduit parallelParams filters' factor)
        
multiLayerMagnitudeVariedSizedConduit
  :: (R.Source s Double)
  => ParallelParams
  -> [PolarSeparableFilterParamsSet]
  -> Int -> Conduit (Array s DIM3 Double) (ResourceT IO) [[VU.Vector Double]]
multiLayerMagnitudeVariedSizedConduit parallelParams filterParamsList factor = do
  xs <- CL.take (batchSize parallelParams)
  unless
    (L.null xs)
    (do let !ys =
              parMapChunk
                parallelParams
                rdeepseq
                (multiLayerMagnitudeVariedSize filterParamsList factor)
                xs
        sourceList ys
        multiLayerMagnitudeVariedSizedConduit parallelParams filterParamsList factor)


{-# INLINE singleLayerMagnitudeFixedSize #-}

singleLayerMagnitudeFixedSize
  :: (R.Source s Double)
  => PolarSeparableFilter PolarSeparableFilterParamsSet (Array U DIM3 (C.Complex Double))
  -> Int
  -> Array s DIM3 Double
  -> Array R.D DIM3 Double
singleLayerMagnitudeFixedSize filter' factor inputArr = downSampledArr
  where
    !filteredArr =
      R.map C.magnitude . applyFilterSetFixedSize filter' . R.map (:+ 0) $ inputArr
    !downSampledArr = RU.downsample [factor, factor, 1] filteredArr

{-# INLINE multiLayerMagnitudeFixedSize #-}

multiLayerMagnitudeFixedSize
  :: (R.Source s Double)
  => [PolarSeparableFilter PolarSeparableFilterParamsSet (Array U DIM3 (C.Complex Double))]
  -> Int
  -> Array s DIM3 Double
  -> [[Vector Double]]
multiLayerMagnitudeFixedSize filters' factor inputArr =
  L.map
    (\arr ->
        let !(Z :. _ :. ny' :. nx') = extent downSampledArr
            !downSampledArr = RU.downsample [factor, factor, 1] arr
        in [ toUnboxed . computeUnboxedS . R.slice downSampledArr $
            (Z :. All :. j :. i)
           | j <- [0 .. ny' - 1]
           , i <- [0 .. nx' - 1] ]) .
  L.tail .
  L.scanl'
    (\arr filter' ->
        R.map C.magnitude . applyFilterSetFixedSize filter' . R.map (:+ 0) $ arr)
    (delay inputArr) $
  filters'

{-# INLINE singleLayerMagnitudeVariedSize #-}

singleLayerMagnitudeVariedSize
  :: (R.Source s Double)
  => PolarSeparableFilterParamsSet
  -> Int
  -> Array s DIM3 Double
  -> Array R.D DIM3 Double
singleLayerMagnitudeVariedSize filterParams factor inputArr = downSampledArr
  where
    !filteredArr =
      R.map C.magnitude . applyFilterSetVariedSize filterParams . R.map (:+ 0) $
      inputArr
    !downSampledArr = RU.downsample [factor, factor, 1] filteredArr

{-# INLINE multiLayerMagnitudeVariedSize #-}

multiLayerMagnitudeVariedSize
  :: (R.Source s Double)
  => [PolarSeparableFilterParamsSet]
  -> Int
  -> Array s DIM3 Double
  -> [[Vector Double]]
multiLayerMagnitudeVariedSize filterParamsList factor inputArr =
  L.map
    (\arr ->
        let !(Z :. _ :. ny' :. nx') = extent downSampledArr
            !downSampledArr = RU.downsample [factor, factor, 1] arr
        in [ toUnboxed . computeUnboxedS . R.slice downSampledArr $
            (Z :. All :. j :. i)
           | j <- [0 .. ny' - 1]
           , i <- [0 .. nx' - 1] ]) .
  L.tail .
  L.scanl'
    (\arr filterParams ->
        R.map C.magnitude . applyFilterSetVariedSize filterParams . R.map (:+ 0) $
        arr)
    (delay inputArr) $
  filterParamsList

{-# INLINE extractPointwiseFeature #-}

extractPointwiseFeature
  :: (R.Source s a, Unbox a)
  => Array s DIM3 a -> [Vector a]
extractPointwiseFeature arr =
  [ toUnboxed . computeUnboxedS . R.slice arr $ (Z :. All :. j :. i)
  | j <- [0 .. ny' - 1]
  , i <- [0 .. nx' - 1] ]
  where
    !(Z :. _ :. ny' :. nx') = extent arr
