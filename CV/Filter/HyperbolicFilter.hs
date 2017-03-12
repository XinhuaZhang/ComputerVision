{-# LANGUAGE BangPatterns #-}
module CV.Filter.HyperbolicFilter where

import           Control.DeepSeq
import           CV.Filter.GaussianFilter
import           Data.Complex             as C
import           Data.List                as L
import           Data.Vector.Unboxed      as VU
import           CV.Utility.Coordinates

data HyperbolicFilterParams = HyperbolicFilterParams
  { getHyperbolicFilterSize             :: !(Int, Int)   -- (nx,ny)
  , getHyperbolicFilterDownsampleFactor :: !Int
  , getHyperbolicFilterScale            :: ![Double]
  , getHyperbolicFilterFreq             :: ![Double]
  , getHyperbolicFilterAngle            :: ![Double]
  } deriving (Show)

instance NFData HyperbolicFilterParams where
  rnf !_ = ()

data HyperbolicFilter = HyperbolicFilter
  { getHyperbolicFilterParams :: HyperbolicFilterParams
  , getHyperbolicFilter       :: [VU.Vector (Complex Double)]
  }
  
instance NFData HyperbolicFilter where
  rnf !_ = ()

makeFilter :: HyperbolicFilterParams -> HyperbolicFilter
makeFilter params@(HyperbolicFilterParams (nx, ny) downsampleFactor scales freqs angles) =
  HyperbolicFilter
    params
    [ VU.fromListN
       (newNx * newNy)
       [ hyperbolic scale angle freq (x - centerX) (y - centerY)
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

{-# INLINE hyperbolic #-}

hyperbolic :: Double -> Double -> Double -> Int -> Int -> Complex Double
hyperbolic scale theta freq x y =
  (0 :+ gaussian2D scale x y) * exp (0 :+ freq * (sqrt . abs $! (u * v)))
  where
    c = cos theta
    s = sin theta
    u = fromIntegral x * c - fromIntegral y * s
    v = fromIntegral x * s + fromIntegral y * c
