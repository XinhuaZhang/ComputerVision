{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE TypeFamilies      #-}
module CV.Filter.PolarSeparableFilter where

import           CV.CUDA.FFT
import           CV.Filter
import           CV.Filter.GaussianFilter
import           CV.Image                              as IM
import           CV.Utility.Coordinates
import           Data.Array.Accelerate                 as A
import           Data.Array.Accelerate.Data.Complex    as A
import           Data.Array.Accelerate.Math.DFT.Centre as A
import           Data.Array.Accelerate.Math.FFT        as A
import           Data.Array.Unboxed                    as AU
import           Data.Complex                          as C
import           Data.List                             as L
import           GHC.Float
import           Prelude                               as P

data PolarSeparableFilterName
  = Fans
  | Bullseye
  | Pinwheels
  deriving (Show,Read)

data PolarSeparableFilterParams = PolarSeparableFilterParams
  { getRadius      :: Int
  , getScale       :: [Double]
  , getRadialFreq  :: [Int]
  , getAngularFreq :: [Int]
  , getName        :: PolarSeparableFilterName
  } deriving (Show)

data PolarSeparableFilter a = PolarSeparableFilter
  { getParams :: PolarSeparableFilterParams
  , getFilter :: a
  }

instance Filter (PolarSeparableFilter (Acc (A.Array DIM3 (A.Complex Double)))) where
  type Input (PolarSeparableFilter (Acc (A.Array DIM3 (A.Complex Double)))) = Acc (A.Array DIM2 Double)
  type Output (PolarSeparableFilter (Acc (A.Array DIM3 (A.Complex Double)))) = Acc (A.Array DIM3 (A.Complex Double))
  type FilterParameter (PolarSeparableFilter (Acc (A.Array DIM3 (A.Complex Double)))) = PolarSeparableFilterParams
  {- Default format HWD -}
  makeFilter
    :: PolarSeparableFilterParams
    -> (PolarSeparableFilter (Acc (A.Array DIM3 (A.Complex Double))))
  makeFilter params@(PolarSeparableFilterParams r scale rs as name) =
    (PolarSeparableFilter params filterArr)
    where
      size = 2 * r
      filterEleList =
        [ pixelList
           (IM.makeFilter size size (getFilterFunc params s rf af) :: ComplexImage)
        | rf <- rs
        , af <- as
        , s <- scale ]
      len = P.length filterEleList
      filterArr =
        (centre25D HWD >-> fft25D' A.Forward size size len HWD) .
        use . A.fromList (Z :. size :. size :. len) . P.concat . L.transpose $
        filterEleList
  displayFilter (PolarSeparableFilter _ imgAcc) = undefined
  applyFilter
    :: PolarSeparableFilter (Acc (A.Array DIM3 (A.Complex Double)))
    -> Acc (A.Array DIM2 Double)
    -> Acc (A.Array DIM3 (A.Complex Double))
  applyFilter (PolarSeparableFilter params@PolarSeparableFilterParams {getRadius = r} filterArr) imgAcc =
    fft25D' A.Inverse size size len HWD $ A.zipWith (*) imgArr filterArr
    where
      size = 2 * r
      len = getFilterNum params
      imgArr =
        A.replicate (lift (Z :. All :. All :. len)) .
        A.fft2D' A.Forward size size . centre2D . A.map (\x -> lift $ x C.:+ 0) $
        imgAcc
        
instance Filter (PolarSeparableFilter (Acc (A.Array DIM3 (A.Complex Float)))) where
  type Input (PolarSeparableFilter (Acc (A.Array DIM3 (A.Complex Float)))) = Acc (A.Array DIM2 Float)
  type Output (PolarSeparableFilter (Acc (A.Array DIM3 (A.Complex Float)))) = Acc (A.Array DIM3 (A.Complex Float))
  type FilterParameter (PolarSeparableFilter (Acc (A.Array DIM3 (A.Complex Float)))) = PolarSeparableFilterParams
  {- Default format HWD -}
  makeFilter
    :: PolarSeparableFilterParams
    -> (PolarSeparableFilter (Acc (A.Array DIM3 (A.Complex Float))))
  makeFilter params@(PolarSeparableFilterParams r scale rs as name) =
    (PolarSeparableFilter params filterArr)
    where
      size = 2 * r
      filterEleList =
        [ pixelList
           (IM.makeFilter size size (getFilterFunc params s rf af) :: ComplexImage)
        | rf <- rs
        , af <- as
        , s <- scale ]
      len = P.length filterEleList
      filterArr =
        (centre25D HWD >-> fft25D' A.Forward size size len HWD) .
        use .
        A.fromList (Z :. size :. size :. len) .
        P.map (\(x C.:+ y) -> (double2Float x C.:+ double2Float y)) .
        P.concat . L.transpose $
        filterEleList
  displayFilter (PolarSeparableFilter _ imgAcc) = undefined
  applyFilter
    :: PolarSeparableFilter (Acc (A.Array DIM3 (A.Complex Float)))
    -> Acc (A.Array DIM2 Float)
    -> Acc (A.Array DIM3 (A.Complex Float))
  applyFilter (PolarSeparableFilter params@PolarSeparableFilterParams {getRadius = r} filterArr) imgAcc =
    fft25D' A.Inverse size size len HWD $ A.zipWith (*) imgArr filterArr
    where
      size = 2 * r
      len = getFilterNum params
      imgArr =
        A.replicate (lift (Z :. All :. All :. len)) .
        A.fft2D' A.Forward size size . centre2D . A.map (\x -> lift $ x C.:+ 0) $
        imgAcc


{- e^jx -}
ejx
  :: (RealFloat a)
  => a -> C.Complex a
ejx x = exp (0 C.:+ x)

real2Complex
  :: (RealFloat a)
  => a -> C.Complex a
real2Complex x = x C.:+ 0

angularFunc :: Int -> PixelOp (Pixel ComplexImage)
angularFunc freq =
  \x y ->
     ejx
       ((P.fromIntegral freq) *
        (angleFunctionRad (P.fromIntegral x) (P.fromIntegral y)))

radialFunc :: Int -> PixelOp (Pixel ComplexImage)
radialFunc freq =
  \x y ->
     ejx
       ((1 - exp (-1 * P.fromIntegral freq / 8)) *
        (sqrt . P.fromIntegral $ x ^ 2 + y ^ 2) *
        pi)

fans :: Double -> Int -> Int -> PixelOp (C.Complex Double)
fans scale _rf af x y
  | scale == 0 = (angularFunc af x y)
  | otherwise = (angularFunc af x y) * (real2Complex (gaussian2D scale x y))

bullseye :: Double
         -> Int
         -> Int
         -> PixelOp (C.Complex Double)
bullseye scale rf _af x y
  | scale == 0 = (radialFunc rf x y)
  | otherwise = (radialFunc rf x y) * (real2Complex (gaussian2D scale x y))

pinwheels :: Double
          -> Int
          -> Int
          -> PixelOp (C.Complex Double)
pinwheels scale rf af x y
  | scale == 0 = (real2Complex (gaussian2D scale x y)) * (angularFunc af x y)
  | otherwise =
    (real2Complex (gaussian2D scale x y)) * (angularFunc af x y) *
    (radialFunc rf x y)

getFilterFunc :: PolarSeparableFilterParams
              -> (Double  -> Int -> Int -> PixelOp (C.Complex Double))
getFilterFunc PolarSeparableFilterParams {getName = Fans} = fans 
getFilterFunc PolarSeparableFilterParams {getName = Bullseye} = bullseye 
getFilterFunc PolarSeparableFilterParams {getName = Pinwheels} = pinwheels 

getFilterNum :: PolarSeparableFilterParams -> Int
getFilterNum (PolarSeparableFilterParams _ scale rs as _) =
  (P.product . P.map P.length $ [rs, as]) * P.length scale
