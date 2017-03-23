{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
module CV.Filter.HyperbolicFilter where

import           Control.DeepSeq
import           CV.Filter.GaussianFilter
import           CV.FilterExpansion
import           CV.Utility.Coordinates
import           Data.Complex             as C
import           Data.List                as L
import           Data.Vector.Unboxed      as VU


data HyperbolicFilterParams = HyperbolicFilterParams
  { getHyperbolicFilterGridRows         :: !Int
  , getHyperbolicFilterGridCols         :: !Int
  , getHyperbolicFilterRows             :: !Int
  , getHyperbolicFilterCols             :: !Int
  , getHyperbolicFilterDownsampleFactor :: !Int
  , getHyperbolicFilterScale            :: ![Double]
  , getHyperbolicFilterFreq             :: ![Double]
  , getHyperbolicFilterAngle            :: ![Double]
  } deriving (Show)

instance NFData HyperbolicFilterParams where
  rnf !_ = ()

data HyperbolicFilter = HyperbolicFilter
  { getHyperbolicFilterParams :: HyperbolicFilterParams
  , getHyperbolicFilter       :: [[VU.Vector (Complex Double)]]
  }

instance NFData HyperbolicFilter where
  rnf !_ = ()

instance FilterExpansion HyperbolicFilter where
  type FilterParameter HyperbolicFilter = HyperbolicFilterParams
  {-# INLINE makeFilter #-}
  makeFilter (HyperbolicFilter params@(HyperbolicFilterParams gRows gCols rows cols downsampleFactor scales freqs angles) _) =
    HyperbolicFilter params .
    L.map
      (\(centerR, centerC) ->
          [ VU.fromListN
             (newCols * newRows)
             [ hyperbolic scale angle freq (x - centerC) (y - centerR)
             | y <- [0 .. newRows - 1]
             , x <- [0 .. newCols - 1] ]
          | angle <- radAngles
          , scale <- scales
          , freq <- freqs ]) $
    grid2D (newRows, newCols) (gRows, gCols)
    where
      newCols = div cols downsampleFactor
      newRows = div rows downsampleFactor
      radAngles = L.map deg2Rad angles
  getFilterSize (HyperbolicFilter (HyperbolicFilterParams gRows gCols _ _ _ scales fs as) _) =
    gRows * gCols * (L.product . L.map L.length $ [scales, fs, as])
  getFilterParameter = getHyperbolicFilterParams
  {-# INLINE getFilterVectors #-}
  getFilterVectors (HyperbolicFilter _ vecs) = vecs
  {-# INLINE changeSizeParameter #-}
  changeSizeParameter rows cols (HyperbolicFilter (HyperbolicFilterParams gRows gCols _ _ df scale freq angle) vecs) =
    HyperbolicFilter
      (HyperbolicFilterParams gRows gCols rows cols df scale freq angle)
      vecs

{-# INLINE hyperbolic #-}

hyperbolic :: Double -> Double -> Double -> Int -> Int -> Complex Double
hyperbolic scale theta freq x y =
  (0 :+ gaussian2D scale x y) * exp (0 :+ freq * (sqrt . abs $! (u * v)))
  where
    c = cos theta
    s = sin theta
    u = fromIntegral x * c - fromIntegral y * s
    v = fromIntegral x * s + fromIntegral y * c
