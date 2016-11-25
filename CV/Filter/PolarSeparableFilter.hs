{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE TypeFamilies      #-}
module CV.Filter.PolarSeparableFilter where

import           CV.Filter
import           CV.Filter.GaussianFilter
import           CV.Image                 as IM
import           CV.Utility.Coordinates
import           Data.Array.Unboxed       as AU
import           Data.Complex             as C
import           Data.Set                 as Set
import           Prelude                  as P

data PolarSeparableFilterName
  = Fans
  | Bullseye
  | Pinwheels
  deriving (Show,Read)

data PolarSeparableFilterParams = PolarSeparableFilterParams
  { getRadius      :: Int
  , getScale       :: Set Double
  , getRadialFreq  :: Set Int
  , getAngularFreq :: Set Int
  , getName        :: PolarSeparableFilterName
  } deriving (Show)

data PolarSeparableFilter a = PolarSeparableFilter
  { getParams :: PolarSeparableFilterParams
  , getFilter :: a
  }

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
     (sqrt . P.fromIntegral $ x ^ 2 + y ^ 2) *
     pi)

{-# INLINE fans #-}
fans :: Double -> Int -> Int -> PixelOp (C.Complex Double)
fans scale _rf af x y
  | scale == 0 = angularFunc af x y
  | otherwise = angularFunc af x y * real2Complex (gaussian2D scale x y)

{-# INLINE bullseye #-}
bullseye :: Double
         -> Int
         -> Int
         -> PixelOp (C.Complex Double)
bullseye scale rf _af x y
  | scale == 0 = radialFunc rf x y
  | otherwise = radialFunc rf x y * real2Complex (gaussian2D scale x y)

{-# INLINE pinwheels #-}
pinwheels :: Double
          -> Int
          -> Int
          -> PixelOp (C.Complex Double)
pinwheels scale rf af x y
  | scale == 0 = real2Complex (gaussian2D scale x y) * angularFunc af x y
  | otherwise =
    real2Complex (gaussian2D scale x y) * angularFunc af x y * radialFunc rf x y

{-# INLINE getFilterFunc #-}
getFilterFunc :: PolarSeparableFilterParams
              -> (Double  -> Int -> Int -> PixelOp (C.Complex Double))
getFilterFunc PolarSeparableFilterParams {getName = Fans}      = fans
getFilterFunc PolarSeparableFilterParams {getName = Bullseye}  = bullseye
getFilterFunc PolarSeparableFilterParams {getName = Pinwheels} = pinwheels

getFilterNum :: PolarSeparableFilterParams -> Int
getFilterNum (PolarSeparableFilterParams _ scale rs as _) =
  (P.product . P.map Set.size $ [rs, as]) * Set.size scale

{- HWD format -}
slice2D :: AU.Array (Int, Int, Int) a -> Int -> AU.Array (Int, Int) a
slice2D arr featureIdx =
  array
    arrRange
    [ (idx, (\(j, i) -> arr AU.! (j, i, featureIdx)) idx)
    | idx <- range arrRange ]
  where
    ((0, 0, 0), (ny, nx, nf)) = bounds arr
    arrRange = ((0, 0), (ny, nx))

{- slice the outmost dimension -}
slice1D :: AU.Array (Int,Int,Int) a -> [[a]]
slice1D arr =
  [ [ arr AU.! (j, i, k)
    | k <- [0 .. nf] ]
  | (j, i) <- range twoDRange ]
  where
    ((0, 0, 0), (ny, nx, nf)) = bounds arr
    twoDRange = ((0, 0), (ny, nx))
