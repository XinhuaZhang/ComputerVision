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
  -> Conduit (Array s DIM3 Double) (ResourceT IO) (Array U DIM3 Double)
singleLayerMagnitudeFixedSizedConduit parallelParams filter' = do
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
                          singleLayerMagnitudeFixedSize filter' $
                          x'
                    in deepSeqArray y' y')
                xs
        sourceList ys
        singleLayerMagnitudeFixedSizedConduit parallelParams filter')

singleLayerMagnitudeVariedSizedConduit
  :: (R.Source s Double)
  => ParallelParams
  -> PolarSeparableFilterParamsSet
  ->  Conduit (Array s DIM3 Double) (ResourceT IO) (Array U DIM3 Double)
singleLayerMagnitudeVariedSizedConduit parallelParams filterParams = do
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
                          singleLayerMagnitudeVariedSize filterParams  $
                          x'
                    in deepSeqArray y' y')
                xs
        sourceList ys
        singleLayerMagnitudeVariedSizedConduit parallelParams filterParams )
        

singleLayerComplexFixedSizedConduit
  :: (R.Source s Double)
  => ParallelParams
  -> PolarSeparableFilter PolarSeparableFilterParamsSet (Array U DIM3 (C.Complex Double))
  -> Conduit (Array s DIM3 Double) (ResourceT IO) (Array U DIM3 Double)
singleLayerComplexFixedSizedConduit parallelParams filter' = do
  xs <- CL.take (batchSize parallelParams)
  unless
    (L.null xs)
    (do let !ys =
              parMapChunk
                parallelParams
                rseq
                (\x' ->
                    let !y' = singleLayerComplexFixedSize filter' x'
                    in deepSeqArray y' y')
                xs
        sourceList ys
        singleLayerComplexFixedSizedConduit parallelParams filter')

singleLayerComplexVariedSizedConduit
  :: (R.Source s Double)
  => ParallelParams
  -> PolarSeparableFilterParamsSet
  ->  Conduit (Array s DIM3 Double) (ResourceT IO) (Array U DIM3 Double)
singleLayerComplexVariedSizedConduit parallelParams filterParams = do
  xs <- CL.take (batchSize parallelParams)
  unless
    (L.null xs)
    (do let !ys =
              parMapChunk
                parallelParams
                rseq
                (\x' ->
                    let !y' = singleLayerComplexVariedSize filterParams x'
                    in deepSeqArray y' y')
                xs
        sourceList ys
        singleLayerComplexVariedSizedConduit parallelParams filterParams)

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
        


multiLayerMagnitudeFixedSizedConduit1
  :: ParallelParams
  -> [PolarSeparableFilter PolarSeparableFilterParamsSet (Array U DIM3 (C.Complex Double))]
  -> Int
  -> Conduit (LabeledArray DIM3 Double) (ResourceT IO) (Int, [R.Array U DIM3 Double])
multiLayerMagnitudeFixedSizedConduit1 parallelParams filters' factor = do
  xs <- CL.take (batchSize parallelParams)
  unless
    (L.null xs)
    (do let !ys =
              parMapChunk
                parallelParams
                rseq
                (\(LabeledArray label arr') ->
                    let arrs = multiLayerMagnitudeFixedSize1 filters' factor arr'
                    in deepSeqArrays arrs (label, arrs))
                xs
        sourceList ys
        multiLayerMagnitudeFixedSizedConduit1 parallelParams filters' factor)

multiLayerMagnitudeVariedSizedConduit1
  :: ParallelParams
  -> [PolarSeparableFilterParamsSet]
  -> Int
  -> Conduit (LabeledArray DIM3 Double) (ResourceT IO) (Int, [R.Array U DIM3 Double])
multiLayerMagnitudeVariedSizedConduit1 parallelParams filterParamsList factor = do
  xs <- CL.take (batchSize parallelParams)
  unless
    (L.null xs)
    (do let !ys =
              parMapChunk
                parallelParams
                rseq
                (\(LabeledArray label arr') ->
                    let arrs =
                          multiLayerMagnitudeVariedSize1
                            filterParamsList
                            factor
                            arr'
                    in deepSeqArrays arrs (label, arrs))
                xs
        sourceList ys
        multiLayerMagnitudeVariedSizedConduit1 parallelParams filterParamsList factor)



multiLayerComplexFixedSizedConduit
  :: ParallelParams
  -> [PolarSeparableFilter PolarSeparableFilterParamsSet (Array U DIM3 (C.Complex Double))]
  -> Int -> Conduit (LabeledArray DIM3 Double) (ResourceT IO) (Int, [[VU.Vector Double]])
multiLayerComplexFixedSizedConduit parallelParams filters' factor = do
  xs <- CL.take (batchSize parallelParams)
  unless
    (L.null xs)
    (do let !ys =
              parMapChunk
                parallelParams
                rdeepseq
                (\(LabeledArray label arr') ->
                    (label, multiLayerComplexFixedSize filters' factor arr'))
                xs
        sourceList ys
        multiLayerComplexFixedSizedConduit parallelParams filters' factor)

multiLayerComplexVariedSizedConduit
  :: ParallelParams
  -> [PolarSeparableFilterParamsSet]
  -> Int
  -> Conduit (LabeledArray DIM3 Double) (ResourceT IO) (Int, [[VU.Vector Double]])
multiLayerComplexVariedSizedConduit parallelParams filterParamsList factor = do
  xs <- CL.take (batchSize parallelParams)
  unless
    (L.null xs)
    (do let !ys =
              parMapChunk
                parallelParams
                rdeepseq
                (\(LabeledArray label arr') ->
                    (label, multiLayerComplexVariedSize filterParamsList factor arr'))
                xs
        sourceList ys
        multiLayerComplexVariedSizedConduit parallelParams filterParamsList factor)

{-# INLINE singleLayerMagnitudeFixedSize #-}

singleLayerMagnitudeFixedSize
  :: (R.Source s Double)
  => PolarSeparableFilter PolarSeparableFilterParamsSet (Array U DIM3 (C.Complex Double))
  -> Array s DIM3 Double
  -> Array R.D DIM3 Double
singleLayerMagnitudeFixedSize filter' =
  R.map C.magnitude . applyFilterSetFixedSize filter' . R.map (:+ 0) 
  
{-# INLINE singleLayerComplexFixedSize #-}

singleLayerComplexFixedSize
  :: (R.Source s Double)
  => PolarSeparableFilter PolarSeparableFilterParamsSet (Array U DIM3 (C.Complex Double))
  -> Array s DIM3 Double
  -> Array U DIM3 Double
singleLayerComplexFixedSize filter' =
  complexArray2RealArray . applyFilterSetFixedSize filter' . R.map (:+ 0) 


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
        in [ toUnboxed . computeUnboxedS . R.slice downSampledArr $
            (Z :. All :. j :. i)
           | j <- [0 .. ny' - 1]
           , i <- [0 .. nx' - 1] ]) .
  L.tail .
  L.scanl'
    (\arr' filter' ->
        R.map C.magnitude . applyFilterSetFixedSize filter' . R.map (:+ 0) $ arr')
    (delay inputArr) $
  filters'
  

{-# INLINE multiLayerMagnitudeFixedSize1 #-}

multiLayerMagnitudeFixedSize1
  :: (R.Source s Double)
  => [PolarSeparableFilter PolarSeparableFilterParamsSet (Array U DIM3 (C.Complex Double))]
  -> Int
  -> Array s DIM3 Double
  -> [Array U DIM3 Double]
multiLayerMagnitudeFixedSize1 filters' factor inputArr =
  L.map
    (\arr' ->
        let !downSampledArr = RU.downsample [factor, factor, 1] arr'
        in computeUnboxedS downSampledArr) .
  L.tail .
  L.scanl'
    (\arr' filter' ->
        R.map C.magnitude . applyFilterSetFixedSize filter' . R.map (:+ 0) $ arr')
    (delay inputArr) $
  filters'
  

{-# INLINE multiLayerComplexFixedSize #-}

multiLayerComplexFixedSize
  :: (R.Source s Double)
  => [PolarSeparableFilter PolarSeparableFilterParamsSet (Array U DIM3 (C.Complex Double))]
  -> Int
  -> Array s DIM3 Double
  -> [[Vector Double]]
multiLayerComplexFixedSize filters' factor inputArr =
  L.map
    (\arr' ->
        let !(Z :. (_nf' :: Int) :. (ny' :: Int) :. (nx' :: Int)) = extent downSampledArr
            !downSampledArr =
              complexArray2RealArray $
              if factor == 1
                then arr'
                else RU.downsample [factor, factor, 1] arr'
        in [ toUnboxed . computeUnboxedS . R.slice downSampledArr $
            (Z :. All :. j :. i)
           | j <- [0 .. ny' - 1]
           , i <- [0 .. nx' - 1] ]) .
  L.tail .
  L.scanl'
    (\arr' filter' ->
        applyFilterSetFixedSize filter' . R.map (:+ 0) . R.map C.magnitude $ arr')
    (R.map (:+ 0) inputArr) $
  filters'

{-# INLINE singleLayerMagnitudeVariedSize #-}

singleLayerMagnitudeVariedSize
  :: (R.Source s Double)
  => PolarSeparableFilterParamsSet
  -> Array s DIM3 Double
  -> Array R.D DIM3 Double
singleLayerMagnitudeVariedSize filterParams =
  R.map C.magnitude . applyFilterSetVariedSize filterParams . R.map (:+ 0) 
  

{-# INLINE singleLayerComplexVariedSize #-}

singleLayerComplexVariedSize
  :: (R.Source s Double)
  => PolarSeparableFilterParamsSet
  -> Array s DIM3 Double
  -> Array U DIM3 Double
singleLayerComplexVariedSize filterParams =
  complexArray2RealArray . applyFilterSetVariedSize filterParams . R.map (:+ 0) 

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
        in [ toUnboxed . computeUnboxedS . R.slice downSampledArr $
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
  

{-# INLINE multiLayerMagnitudeVariedSize1 #-}

multiLayerMagnitudeVariedSize1
  :: (R.Source s Double)
  => [PolarSeparableFilterParamsSet]
  -> Int
  -> Array s DIM3 Double
  -> [Array U DIM3 Double]
multiLayerMagnitudeVariedSize1 filterParamsList factor inputArr =
  L.map
    (\arr' ->
        let !downSampledArr = RU.downsample [factor, factor, 1] arr'
        in computeUnboxedS downSampledArr) .
  L.tail .
  L.scanl'
    (\arr' filterParams ->
        R.map C.magnitude . applyFilterSetVariedSize filterParams . R.map (:+ 0) $
        arr')
    (delay inputArr) $
  filterParamsList
  

{-# INLINE multiLayerComplexVariedSize #-}

multiLayerComplexVariedSize
  :: (R.Source s Double)
  => [PolarSeparableFilterParamsSet]
  -> Int
  -> Array s DIM3 Double
  -> [[Vector Double]]
multiLayerComplexVariedSize filterParamsList factor inputArr =
  L.map
    (\arr' ->
        let !(Z :. _ :. (ny' :: Int) :. (nx' :: Int)) = extent downSampledArr
            !downSampledArr =
              complexArray2RealArray $
              if factor == 1
                then arr'
                else RU.downsample [factor, factor, 1] arr'
        in [ toUnboxed . computeUnboxedS . R.slice downSampledArr $
            (Z :. All :. j :. i)
           | j <- [0 .. ny' - 1]
           , i <- [0 .. nx' - 1] ]) .
  L.tail .
  L.scanl'
    (\arr' filterParams ->
        applyFilterSetVariedSize filterParams . R.map (:+ 0) . R.map C.magnitude $
        arr')
    (R.map (:+ 0) inputArr) $
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
        

{-# INLINE l2normVec #-}
l2normVec :: VU.Vector Double -> VU.Vector Double
l2normVec vec' 
  | norm == 0 = vec'
  | otherwise = VU.map (/norm2) vec2
  where
    !vec = VU.map (\x' -> if x' < 10 ** (-10)
                             then 0
                             else x') vec'
    !norm = sqrt . VU.sum . VU.map (^ (2 :: Int)) $ vec
    !vec1 = VU.map (/ norm) vec
    !vec2 = VU.map (\x' -> if x' > 0.2
                              then 0.2
                              else x') vec1
    !norm2 = sqrt . VU.sum . VU.map (^ (2 :: Int)) $ vec2

{-# INLINE complexArray2RealArray #-}

complexArray2RealArray
  :: (R.Source s (Complex Double))
  => R.Array s DIM3 (Complex Double) -> R.Array U DIM3 Double
complexArray2RealArray arr =
  fromUnboxed (Z :. 2 * nf' :. ny' :. nx') . VU.concat $
  [VU.map realPart vec, VU.map imagPart vec]
  where
    (Z :. nf' :. ny' :. nx') = extent arr
    vec = toUnboxed . computeUnboxedS . delay $ arr
