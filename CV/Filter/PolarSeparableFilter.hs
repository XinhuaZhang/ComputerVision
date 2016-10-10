module CV.Filter.PolarSeparableFilter where

import           CV.Filter
import           CV.Filter.GaussianFilter
import           CV.Image
import           CV.Utility.Coordinates
import           Data.Complex             as C
import           Prelude                  as P

data PolarSeparableFilterName
  = Fans
  | Bullseye
  | Pinwheels
  deriving (Show,Read)

data PolarSeparableFilterFreq = PolarSeparableFilterFreq
  { getRadialFreq  :: [Int]
  , getAngularFreq :: [Int]
  } deriving (Show)

data PolarSeparableFilterParams =
  PolarSeparableFilterParams {getRadius :: Int
                             ,getScale  :: Double
                             ,getFreq   :: PolarSeparableFilterFreq
                             ,getName   :: PolarSeparableFilterName}
  deriving (Show)

data PolarSeparableFilter a = PolarSeparableFilter
  { getParams :: PolarSeparableFilterParams
  , getFilter :: a
  }

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

fans :: PolarSeparableFilterParams -> Int -> Int -> PixelOp (C.Complex Double)
fans params _rf af x y =
  (angularFunc af x y) * (real2Complex (gaussian2D scale x y))
  where
    scale = getScale params

bullseye :: PolarSeparableFilterParams
         -> Int
         -> Int
         -> PixelOp (C.Complex Double)
bullseye params rf _af x y =
  (radialFunc rf x y) * (real2Complex (gaussian2D scale x y))
  where
    scale = getScale params

pinwheels :: PolarSeparableFilterParams
          -> Int
          -> Int
          -> PixelOp (C.Complex Double)
pinwheels params rf af x y =
  (real2Complex (gaussian2D scale x y)) * (angularFunc af x y) *
  (radialFunc rf x y)
  where
    scale = getScale params
