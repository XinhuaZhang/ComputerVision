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
  deriving (Show, Read)

data PolarSeparableFilterParamsSet = PolarSeparableFilterParamsSet
  { getSizeSet            :: !(Int, Int)
  , getDowsampleFactorSet :: !Int
  , getScaleSet           :: !(Set Double)
  , getRadialFreqSet      :: !(Set Int)
  , getAngularFreqSet     :: !(Set Int)
  , getNameSet            :: !PolarSeparableFilterName
  } deriving (Show)

data PolarSeparableFilter a =
  PolarSeparableFilter !PolarSeparableFilterParams
                       a
  deriving (Show)

data PolarSeparableFilterParams = PolarSeparableFilterParams
  { getSize             :: !(Int, Int)
  , getDownsampleFactor :: !Int
  , getScale            :: !Double
  , getRadialFreq       :: !Int
  , getAngularFreq      :: !Int
  , getName             :: !PolarSeparableFilterName
  } deriving (Show)

generatePSFParamsSet :: PolarSeparableFilterParamsSet
                     -> [PolarSeparableFilterParams]
generatePSFParamsSet (PolarSeparableFilterParamsSet (ny, nx) downsampleFactor scaleSet rfSet afSet name) =
  [ PolarSeparableFilterParams (ny, nx) downsampleFactor scale rf af name
  | scale <- toAscList scaleSet
  , rf <- toAscList rfSet
  , af <- toAscList afSet ]

-- Input: [layer1, layer2 ...]
generateMultilayerPSFParamsSet :: [PolarSeparableFilterParamsSet]
                               -> [[PolarSeparableFilterParams]]
generateMultilayerPSFParamsSet =
  L.map L.reverse .
  L.foldl'
    (\bss as ->
        [ a : bs
        | bs <- bss
        , a <- as ])
    [] .
  L.map generatePSFParamsSet 
  

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

{-# INLINE radialFunc #-}

radialFunc :: Int -> PixelOp (Pixel ComplexImage)
radialFunc freq x y =
  ejx
    ((1 - exp (-1 * P.fromIntegral freq / 8)) *
     (sqrt . P.fromIntegral $ x ^ (2 :: Int) + y ^ (2 :: Int)) *
     pi)

{-# INLINE fans #-}

fans :: Double -> Int -> Int -> PixelOp (C.Complex Double)
fans scale _rf af x y
  | scale == 0 = angularFunc af x y
  | otherwise = angularFunc af x y * real2Complex (gaussian2D scale x y)

{-# INLINE bullseye #-}

bullseye :: Double -> Int -> Int -> PixelOp (C.Complex Double)
bullseye scale rf _af x y
  | scale == 0 = radialFunc rf x y
  | otherwise = radialFunc rf x y * real2Complex (gaussian2D scale x y)

{-# INLINE pinwheels #-}

pinwheels :: Double -> Int -> Int -> PixelOp (C.Complex Double)
pinwheels scale rf af x y
  | scale == 0 = real2Complex (gaussian2D scale x y) * angularFunc af x y
  | otherwise =
    real2Complex (gaussian2D scale x y) * angularFunc af x y * radialFunc rf x y

{-# INLINE getFilterFunc #-}

getFilterFunc
  :: PolarSeparableFilterParams
  -> (Double -> Int -> Int -> PixelOp (C.Complex Double))
getFilterFunc PolarSeparableFilterParams {getName = Fans} = fans
getFilterFunc PolarSeparableFilterParams {getName = Bullseye} = bullseye
getFilterFunc PolarSeparableFilterParams {getName = Pinwheels} = pinwheels

getFilterNum :: PolarSeparableFilterParamsSet -> Int
getFilterNum (PolarSeparableFilterParamsSet _ _ scale rs as _) =
  (P.product . P.map Set.size $ [rs, as]) * Set.size scale


makeFilter :: PolarSeparableFilterParams -> PolarSeparableFilter (CArray (Int, Int) (C.Complex Double))
makeFilter params@(PolarSeparableFilterParams (ny, nx) downSampleFactor scale rf af _name) =
  PolarSeparableFilter params .
  dft . listArray ((0, 0), (ny' - 1, nx' - 1)) . pixelList $
  (IM.makeFilter ny' nx' (getFilterFunc params scale rf af) :: ComplexImage)
  where
    ny' = div ny downSampleFactor
    nx' = div nx downSampleFactor

displayFilter :: PolarSeparableFilterParams -> ComplexImage
displayFilter params@(PolarSeparableFilterParams (ny, nx) downsampleFactor scale rf af _name) =
  IM.makeImage ny' nx' (getFilterFunc params scale rf af) :: ComplexImage
  where
    ny' = div ny downsampleFactor
    nx' = div nx downsampleFactor

applyFilterFixedSize
  :: (Source s (C.Complex Double))
  => PolarSeparableFilter (CArray (Int, Int) (C.Complex Double))
  -> R.Array s DIM3 (C.Complex Double)
  -> R.Array D DIM3 (C.Complex Double) 
applyFilterFixedSize (PolarSeparableFilter params filter') =
  filterFunc (getDownsampleFactor params) filter'

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
        (PolarSeparableFilterParams (ny, nx) downsampleFactor scale rf af name) :: PolarSeparableFilter (CArray (Int, Int) (C.Complex Double))  

{-# INLINE filterFunc #-}

filterFunc
  :: (Source s (C.Complex Double))
  => Int
  -> CArray (Int, Int) (C.Complex Double)
  -> R.Array s DIM3 (C.Complex Double)
  -> R.Array D DIM3 (C.Complex Double)
filterFunc downsampleFactor filter' inputArr =
  threeDCArray2RArray . idftN [1, 2] . threeDRArray2CArray $ multArr
  where
    !cArr =
      if downsampleFactor == 1
        then threeDRArray2CArray inputArr
        else threeDRArray2CArray $
             RU.downsample [downsampleFactor, downsampleFactor, 1] inputArr
    !dftCArr = dftN [1, 2] cArr
    !rArr = threeDCArray2RArray dftCArr
    !filterArr = twoDCArray2RArray filter'
    !multArr =
      computeUnboxedS $
      traverse2
        rArr
        filterArr
        const
        (\f1 f2 idx@(Z :. _k :. j :. i) -> f1 idx * f2 (Z :. j :. i))

{-# INLINE twoDCArray2RArray #-}

twoDCArray2RArray
  :: (Num a, Storable a)
  => CArray (Int, Int) a -> R.Array D DIM2 a
twoDCArray2RArray cArr =
  fromFunction
    (Z :. (ny' + 1) :. (nx' + 1))
    (\(Z :. j :. i) -> cArr CA.! (j, i))
  where
    ((_, _), (ny', nx')) = bounds cArr

{-# INLINE threeDRArray2CArray #-}

threeDRArray2CArray
  :: (Num a, Storable a, Source s a)
  => R.Array s DIM3 a -> CArray (Int, Int, Int) a
threeDRArray2CArray rArr =
  listArray ((0, 0, 0), (nf - 1, ny - 1, nx - 1)) . R.toList $ rArr
  where
    (Z :. nf :. ny :. nx) = extent rArr

{-# INLINE threeDCArray2RArray #-}

threeDCArray2RArray
  :: (Num a, Storable a)
  => CArray (Int, Int, Int) a -> R.Array D DIM3 a
threeDCArray2RArray cArr =
  fromFunction
    (Z :. (nf' + 1) :. (ny' + 1) :. (nx' + 1))
    (\(Z :. k :. j :. i) -> cArr CA.! (k, j, i))
  where
    ((_, _, _), (nf', ny', nx')) = bounds cArr
