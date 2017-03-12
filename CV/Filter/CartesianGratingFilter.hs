{-# LANGUAGE BangPatterns #-}
module CV.Filter.CartesianGratingFilter where

import           Control.DeepSeq
import           CV.Filter.GaussianFilter
import           Data.Complex             as C
import           Data.List                as L
import           Data.Vector.Unboxed      as VU
import           CV.Utility.Coordinates

data CartesianGratingFilterParams = CartesianGratingFilterParams
  { getCartesianGratingFilterSize             :: !(Int, Int)   -- (nx,ny)
  , getCartesianGratingFilterDownsampleFactor :: !Int
  , getCartesianGratingFilterScale            :: ![Double]
  , getCartesianGratingFilterFreq             :: ![Double]
  , getCartesianGratingFilterAngle            :: ![Double]
  } deriving (Show)

instance NFData CartesianGratingFilterParams where
  rnf !_ = ()

data CartesianGratingFilter = CartesianGratingFilter
  { getCartesianGratingFilterParams :: CartesianGratingFilterParams
  , getCartesianGratingFilter       :: [VU.Vector (Complex Double)]
  }
  
instance NFData CartesianGratingFilter where
  rnf !_ = ()

makeFilter :: CartesianGratingFilterParams -> CartesianGratingFilter
makeFilter params@(CartesianGratingFilterParams (nx, ny) downsampleFactor scales freqs angles) =
  CartesianGratingFilter
    params
    [ VU.fromListN
       (newNx * newNy)
       [ cartesianGrating scale angle freq (x - centerX) (y - centerY)
       | y <- [0 .. newNy - 1]
       , x <- [0 .. newNx - 1] ]
    | angle <- radAngles
    , scale <- scales
    , freq <- freqs ]
  where
    newNx = div nx downsampleFactor
    newNy = div ny downsampleFactor
    centerX = div newNx 2
    centerY = div newNy 2
    radAngles = L.map deg2Rad angles

{-# INLINE cartesianGrating #-}

cartesianGrating :: Double -> Double -> Double -> Int -> Int -> Complex Double
cartesianGrating scale theta freq x y =
  (0 :+ gaussian2D scale x y) * exp (0 :+ freq * u)
  where
    c = cos theta
    s = sin theta
    u = fromIntegral x * c - fromIntegral y * s

