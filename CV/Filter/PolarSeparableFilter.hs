{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}

module CV.Filter.PolarSeparableFilter where

import           CV.Filter.GaussianFilter
import           CV.Image                    as IM
import           CV.Utility.Coordinates
import           CV.Utility.RepaArrayUtility as RU
import           Data.Array.CArray           as CA
import           Data.Array.Repa             as R
import           Data.Complex                as C
import           Data.List                   as L
import           Data.Set                    as Set
import           Foreign.Storable
import           Math.FFT
import           Prelude                     as P

data PolarSeparableFilterName
  = Fans
  | Bullseye
  | Pinwheels
  | Pinwheels180
  deriving (Show, Read)

data PolarSeparableFilterParamsSet = PolarSeparableFilterParamsSet
  { getSizeSet             :: !(Int, Int)
  , getDownsampleFactorSet :: !Int
  , getScaleSet            :: !(Set Double)
  , getRadialFreqSet       :: !(Set Int)
  , getAngularFreqSet      :: !(Set Int)
  , getNameSet             :: !PolarSeparableFilterName
  } deriving (Show)

data PolarSeparableFilter a b =
  PolarSeparableFilter !a
                       b
  deriving (Show)

data PolarSeparableFilterParams = PolarSeparableFilterParams
  { getSize             :: !(Int, Int)
  , getDownsampleFactor :: !Int
  , getScale            :: !Double
  , getRadialFreq       :: !Int
  , getAngularFreq      :: !Int
  , getName             :: !PolarSeparableFilterName
  } deriving (Show)

-- this function is to make sure that the params sequence is correct
{-# INLINE generateParamsSet #-}

generateParamsSet :: Set Double -> Set Int -> Set Int -> [(Double, Int, Int)]
generateParamsSet scaleSet rfSet afSet =
  [ (scale, rf, af)
  | scale <- toAscList scaleSet
  , rf <- toAscList rfSet
  , af <- toAscList afSet ]

generatePSFParamsSet :: PolarSeparableFilterParamsSet
                     -> [PolarSeparableFilterParams]
generatePSFParamsSet (PolarSeparableFilterParamsSet (ny, nx) downsampleFactor scaleSet rfSet afSet name) =
  L.map
    (\(scale, rf, af) ->
        PolarSeparableFilterParams (ny, nx) downsampleFactor scale rf af name) $
  generateParamsSet scaleSet rfSet afSet

-- Input: [layer1, layer2 ...]
generateMultilayerPSFParamsSet :: [PolarSeparableFilterParamsSet]
                               -> [[PolarSeparableFilterParams]]
generateMultilayerPSFParamsSet =
  L.foldl'
    (\bss as ->
        [ a : bs
        | bs <- bss
        , a <- as ])
    [[]] .
  L.map generatePSFParamsSet . L.reverse

{-# INLINE ejx #-}

ejx
  :: (RealFloat a)
  => a -> C.Complex a
ejx x = exp (0 C.:+ x)

{-# INLINE real2Complex #-}

real2Complex
  :: (RealFloat a)
  => a -> C.Complex a
real2Complex x = x C.:+ 0

{-# INLINE angularFunc #-}

angularFunc :: Int -> PixelOp (Pixel ComplexImage)
angularFunc freq x y =
  ejx
    (P.fromIntegral freq *
     angleFunctionRad (P.fromIntegral x) (P.fromIntegral y))

{-# INLINE angularFunc180 #-}

angularFunc180 :: Int -> PixelOp (Pixel ComplexImage)
angularFunc180 freq x y =
  ejx
    (P.fromIntegral freq *
     (angleFunctionRad (P.fromIntegral x) (P.fromIntegral y) + pi))

{-# INLINE radialFunc #-}

radialFunc :: Double -> Int -> PixelOp (Pixel ComplexImage)
radialFunc scale freq x y =
  ejx
    ((sqrt . P.fromIntegral $ x ^ (2 :: Int) + y ^ (2 :: Int)) * fromIntegral freq *
     (2 * pi) /
     (16 * scale))
-- ejx
--   ((1 - exp (-1 * P.fromIntegral freq / 8)) *
--    (sqrt . P.fromIntegral $ x ^ (2 :: Int) + y ^ (2 :: Int)) *
--    pi)

{-# INLINE fans #-}

fans :: Double -> Int -> Int -> PixelOp (C.Complex Double)
fans scale _rf af x y
  | scale == 0 = angularFunc af x y
  | otherwise = angularFunc af x y * real2Complex (gaussian2D scale x y)

{-# INLINE bullseye #-}

bullseye :: Double -> Int -> Int -> PixelOp (C.Complex Double)
bullseye scale rf _af x y
  | scale == 0 = radialFunc scale rf x y
  | otherwise = radialFunc scale rf x y * real2Complex (gaussian2D scale x y)

{-# INLINE pinwheels #-}

pinwheels :: Double -> Int -> Int -> PixelOp (C.Complex Double)
pinwheels scale rf af x y
  | scale == 0 = (angularFunc af x y * radialFunc scale  rf x y) 
  | otherwise =
    (real2Complex (gaussian2DRing af rf scale x y) * angularFunc af x y * radialFunc scale rf x y)
    -- (real2Complex (gaussian2D  scale x y) * angularFunc af x y * radialFunc scale rf x y)

{-# INLINE pinwheels180 #-}

pinwheels180 :: Double -> Int -> Int -> PixelOp (C.Complex Double)
pinwheels180 scale rf af x y
  | scale == 0 = angularFunc af x y * radialFunc scale rf x y
  | otherwise =
    real2Complex (gaussian2DRing af rf scale x y) * angularFunc180 af x y *
    radialFunc scale rf x y

{-# INLINE getFilterFunc #-}

getFilterFunc
  :: PolarSeparableFilterParams
  -> (Double -> Int -> Int -> PixelOp (C.Complex Double))
getFilterFunc PolarSeparableFilterParams {getName = Fans} = fans
getFilterFunc PolarSeparableFilterParams {getName = Bullseye} = bullseye
getFilterFunc PolarSeparableFilterParams {getName = Pinwheels} = pinwheels
getFilterFunc PolarSeparableFilterParams {getName = Pinwheels180} = pinwheels180

{-# INLINE getFilterSetFunc #-}

getFilterSetFunc
  :: PolarSeparableFilterParamsSet
  -> (Double -> Int -> Int -> PixelOp (C.Complex Double))
getFilterSetFunc PolarSeparableFilterParamsSet {getNameSet = Fans} = fans
getFilterSetFunc PolarSeparableFilterParamsSet {getNameSet = Bullseye} = bullseye
getFilterSetFunc PolarSeparableFilterParamsSet {getNameSet = Pinwheels} = pinwheels
getFilterSetFunc PolarSeparableFilterParamsSet {getNameSet = Pinwheels180} = pinwheels180

getFilterNum :: PolarSeparableFilterParamsSet -> Int
getFilterNum (PolarSeparableFilterParamsSet _ _ scale rs as _) =
  (P.product . P.map Set.size $ [rs, as]) * Set.size scale

makeFilter
  :: PolarSeparableFilterParams
  -> PolarSeparableFilter PolarSeparableFilterParams (R.Array U DIM2 (C.Complex Double))
makeFilter params@(PolarSeparableFilterParams (ny, nx) downSampleFactor scale rf af _name) =
  PolarSeparableFilter params .
  computeS .
  twoDCArray2RArray . dftN [0, 1] . listArray ((0, 0), (ny' - 1, nx' - 1)) $
  makeFilterList ny' nx' (getFilterFunc params scale rf af)
  where
    ny' = div ny downSampleFactor
    nx' = div nx downSampleFactor

makeFilterSet
  :: PolarSeparableFilterParamsSet
  -> PolarSeparableFilter PolarSeparableFilterParamsSet (R.Array U DIM3 (C.Complex Double))
makeFilterSet params@(PolarSeparableFilterParamsSet (ny, nx) downSampleFactor scaleSet rfSet afSet _name) =
  PolarSeparableFilter params filterArr
  where
    !paramsList = generateParamsSet scaleSet rfSet afSet
    !filterEleList =
      L.map
        (\(scale, rf, af) ->
            makeFilterList ny' nx' (getFilterSetFunc params scale rf af))
        paramsList
    !ny' = div ny downSampleFactor
    !nx' = div nx downSampleFactor
    !nf' = L.length filterEleList
    !cArr = listArray ((0, 0, 0), (nf' - 1, ny' - 1, nx' - 1)) . L.concat $ filterEleList
    !dftCArr = dftN [1, 2] cArr
    !filterArr = computeS $ threeDCArray2RArray dftCArr
    

makeFlippedFilterSet
  :: PolarSeparableFilterParamsSet
  -> PolarSeparableFilter PolarSeparableFilterParamsSet (CArray (Int,Int,Int) (C.Complex Double))
makeFlippedFilterSet params@(PolarSeparableFilterParamsSet (ny, nx) downSampleFactor scaleSet rfSet afSet _name) =
  PolarSeparableFilter params dftCArr
  where
    paramsList = generateParamsSet scaleSet rfSet afSet
    filterEleList =
      L.concatMap
        (\(scale, rf, af) ->
            makeFilterList ny' nx' (getFilterSetFunc params scale rf af))
        paramsList
    ny' = div ny downSampleFactor
    nx' = div nx downSampleFactor
    nf' = L.length paramsList
    rArr = fromListUnboxed (Z :. nf' :. ny' :. nx') filterEleList
    flippedRArr = R.transpose rArr
    cArr = threeDRArray2CArray flippedRArr
    dftCArr = dftN [1, 2] cArr

displayFilter :: PolarSeparableFilterParams -> ComplexImage
displayFilter params@(PolarSeparableFilterParams (ny, nx) downsampleFactor scale rf af _name) =
  IM.makeImage ny' nx' (getFilterFunc params scale rf af) :: ComplexImage
  where
    ny' = div ny downsampleFactor
    nx' = div nx downsampleFactor

applyFilterFixedSize
  :: (Source s (C.Complex Double))
  => PolarSeparableFilter PolarSeparableFilterParams (R.Array U DIM2 (C.Complex Double))
  -> R.Array s DIM3 (C.Complex Double)
  -> R.Array D DIM3 (C.Complex Double)
applyFilterFixedSize (PolarSeparableFilter params filter') =
  filterFunc (getDownsampleFactor params) filter'

applyFilterSetFixedSize
  :: (Source s (C.Complex Double))
  => PolarSeparableFilter PolarSeparableFilterParamsSet (R.Array U DIM3 (C.Complex Double))
  -> R.Array s DIM3 (C.Complex Double)
  -> R.Array D DIM3 (C.Complex Double)
applyFilterSetFixedSize (PolarSeparableFilter params filter') =
  filterSetFunc (getDownsampleFactorSet params) filter'

applyFilterVariedSize
  :: (Source s (C.Complex Double))
  => PolarSeparableFilterParams
  -> R.Array s DIM3 (C.Complex Double)
  -> R.Array D DIM3 (C.Complex Double)
applyFilterVariedSize (PolarSeparableFilterParams _ downsampleFactor scale rf af name) inputArr =
  filterFunc downsampleFactor filter' inputArr
  where
    (Z :. _ :. ny :. nx) = extent inputArr
    (PolarSeparableFilter _ !filter') =
      CV.Filter.PolarSeparableFilter.makeFilter
        (PolarSeparableFilterParams (ny, nx) downsampleFactor scale rf af name)

applyFilterSetVariedSize
  :: (Source s (C.Complex Double))
  => PolarSeparableFilterParamsSet
  -> R.Array s DIM3 (C.Complex Double)
  -> R.Array D DIM3 (C.Complex Double)
applyFilterSetVariedSize (PolarSeparableFilterParamsSet _ downsampleFactor scaleSet rfSet afSet name) inputArr =
  filterSetFunc downsampleFactor filter' inputArr
  where
    (Z :. _ :. ny :. nx) = extent inputArr
    (PolarSeparableFilter _ !filter') =
      makeFilterSet
        (PolarSeparableFilterParamsSet
           (ny, nx)
           downsampleFactor
           scaleSet
           rfSet
           afSet
           name)

{-# INLINE filterFunc #-}

filterFunc
  :: (Source s (C.Complex Double))
  => Int
  -> R.Array U DIM2 (C.Complex Double)
  -> R.Array s DIM3 (C.Complex Double)
  -> R.Array D DIM3 (C.Complex Double)
filterFunc downsampleFactor filterArr inputArr =
  threeDCArray2RArray . idftN [1, 2] . threeDRArray2CArray $ multArr
  where
    !cArr =
      if downsampleFactor == 1
        then threeDRArray2CArray inputArr
        else threeDRArray2CArray $
             RU.downsample [downsampleFactor, downsampleFactor, 1] inputArr
    !dftCArr = dftN [1, 2] cArr
    !rArr = threeDCArray2RArray dftCArr
    !multArr =
      computeUnboxedS $
      traverse2
        rArr
        filterArr
        const
        (\f1 f2 idx@(Z :. _k :. j :. i) -> f1 idx * f2 (Z :. j :. i))

{-# INLINE filterSetFunc #-}

filterSetFunc
  :: (Source s (C.Complex Double))
  => Int
  -> R.Array U DIM3 (C.Complex Double)
  -> R.Array s DIM3 (C.Complex Double)
  -> R.Array D DIM3 (C.Complex Double)
filterSetFunc downsampleFactor filterArr inputArr =
  threeDCArray2RArray . idftN [1, 2] . threeDRArray2CArray $ multArr
  where
    !cArr =
      if downsampleFactor == 1
        then threeDRArray2CArray inputArr
        else threeDRArray2CArray $
             RU.downsample [downsampleFactor, downsampleFactor, 1] inputArr
    !dftCArr = dftN [1, 2] cArr
    !rArr = threeDCArray2RArray dftCArr
    !(Z :. nfFilter :. ny' :. nx') = extent filterArr
    !(Z :. nfInput :. _ :. _) = extent inputArr
    !newNf = nfFilter * nfInput
    !multArr =
      computeUnboxedS $
      fromFunction
        (Z :. newNf :. ny' :. nx')
        (\(Z :. k :. j :. i) ->
            let !kInput = mod k nfInput
                !kFilter = div k nfInput
            in rArr R.! (Z :. kInput :. j :. i) *
               filterArr R.! (Z :. kFilter :. j :. i))
