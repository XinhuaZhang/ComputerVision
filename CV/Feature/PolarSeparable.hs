{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
  :: ParallelParams
  -> [PolarSeparableFilter PolarSeparableFilterParamsSet (Array U DIM3 (C.Complex Double))]
  -> Int -> Conduit (LabeledArray DIM3 Double) (ResourceT IO) (Int, [[VU.Vector Double]])
multiLayerMagnitudeFixedSizedConduit parallelParams filters' factor = do
  xs <- CL.take (batchSize parallelParams)
  unless
    (L.null xs)
    (do let !ys =
              parMapChunk
                parallelParams
                rdeepseq
                (\(LabeledArray label arr') ->
                    (label, multiLayerMagnitudeFixedSize filters' factor arr'))
                xs
        sourceList ys
        multiLayerMagnitudeFixedSizedConduit parallelParams filters' factor)

multiLayerMagnitudeVariedSizedConduit
  :: ParallelParams
  -> [PolarSeparableFilterParamsSet]
  -> Int
  -> Conduit (LabeledArray DIM3 Double) (ResourceT IO) (Int, [[VU.Vector Double]])
multiLayerMagnitudeVariedSizedConduit parallelParams filterParamsList factor = do
  xs <- CL.take (batchSize parallelParams)
  unless
    (L.null xs)
    (do let !ys =
              parMapChunk
                parallelParams
                rdeepseq
                (\(LabeledArray label arr') ->
                    (label, multiLayerMagnitudeVariedSize filterParamsList factor arr'))
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
    (\arr' ->
        let !(Z :. (_nf'::Int) :. (ny'::Int) :. (nx'::Int)) = extent downSampledArr
            !downSampledArr = RU.downsample [factor, factor, 1] arr'
        in [ l2normVec . toUnboxed . computeUnboxedS . R.slice downSampledArr $
            (Z :. All :. j :. i)
           | j <- [0 .. ny' - 1]
           , i <- [0 .. nx' - 1] ]) .
  L.tail .
  L.scanl'
    (\arr' filter' ->
        R.map C.magnitude . applyFilterSetFixedSize filter' . R.map (:+ 0) $ arr')
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
    (\arr' ->
        let !(Z :. _ :. (ny'::Int) :. (nx'::Int)) = extent downSampledArr
            !downSampledArr = RU.downsample [factor, factor, 1] arr'
        in [ l2normVec . toUnboxed . computeUnboxedS . R.slice downSampledArr $
            (Z :. All :. j :. i)
           | j <- [0 .. ny' - 1]
           , i <- [0 .. nx' - 1] ]) .
  L.tail .
  L.scanl'
    (\arr' filterParams ->
        R.map C.magnitude . applyFilterSetVariedSize filterParams . R.map (:+ 0) $
        arr')
    (delay inputArr) $
  filterParamsList
  
extractPointwiseFeatureConduit
  :: (R.Source s Double)
  => ParallelParams -> Conduit (Array s DIM3 Double) (ResourceT IO) [Vector Double]
extractPointwiseFeatureConduit parallelParams = do
  xs' <- CL.take (batchSize parallelParams)
  unless
    (L.null xs')
    (do let ys' = parMapChunk parallelParams rdeepseq extractPointwiseFeature xs'
        sourceList ys'
        extractPointwiseFeatureConduit parallelParams)

{-# INLINE extractPointwiseFeature #-}

extractPointwiseFeature
  :: (R.Source s Double)
  => Array s DIM3 Double -> [Vector Double]
extractPointwiseFeature arr' =
  [ l2normVec . toUnboxed . computeUnboxedS . R.slice arr' $ (Z :. All :. j :. i)
  | j <- [0 .. ny' - 1]
  , i <- [0 .. nx' - 1] ]
  where
    !(Z :. _ :. (ny'::Int) :. (nx'::Int)) = extent arr'

{-# INLINE l2normVec #-}
l2normVec :: VU.Vector Double -> VU.Vector Double
l2normVec vec 
  | norm == 0 = vec
  | otherwise = VU.map (/norm2) vec2
  where
    !norm = sqrt . VU.sum . VU.map (^ (2 :: Int)) $ vec
    !vec1 = VU.map (/ norm) vec
    !vec2 = VU.map (\x -> if x > 0.2
                             then 0.2
                             else x) vec1
    !norm2 = sqrt . VU.sum . VU.map (^ (2 :: Int)) $ vec2
