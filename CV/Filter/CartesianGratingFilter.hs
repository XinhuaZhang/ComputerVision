{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
module CV.Filter.CartesianGratingFilter where

import           Control.DeepSeq
import           CV.Filter.GaussianFilter
import           CV.FilterExpansion
import           CV.Utility.Coordinates
import           Data.Complex             as C
import           Data.List                as L
import           Data.Vector.Unboxed      as VU

data CartesianGratingFilterParams = CartesianGratingFilterParams
  { getCartesianGratingFilterGridRows         :: !Int
  , getCartesianGratingFilterGridCols         :: !Int
  , getCartesianGratingFilterRows             :: !Int
  , getCartesianGratingFilterCols             :: !Int
  , getCartesianGratingFilterDownsampleFactor :: !Int
  , getCartesianGratingFilterScale            :: ![Double]
  , getCartesianGratingFilterFreq             :: ![Double]
  , getCartesianGratingFilterAngle            :: ![Double]
  } deriving (Show)

instance NFData CartesianGratingFilterParams where
  rnf !_ = ()

data CartesianGratingFilter = CartesianGratingFilter
  { getCartesianGratingFilterParams :: CartesianGratingFilterParams
  , getCartesianGratingFilter       :: [[VU.Vector (Complex Double)]]
  }

instance NFData CartesianGratingFilter where
  rnf !_ = ()

instance FilterExpansion CartesianGratingFilter where
  type FilterParameter CartesianGratingFilter = CartesianGratingFilterParams
  {-# INLINE makeFilter #-}
  makeFilter (CartesianGratingFilter params@(CartesianGratingFilterParams gRows gCols rows cols downsampleFactor scales freqs angles) _) =
    CartesianGratingFilter params .
    L.map
      (\(centerR, centerC) ->
          [ VU.fromListN
             (newCols * newRows)
             [ cartesianGrating scale angle freq (x - centerC) (y - centerR)
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
  getFilterSize (CartesianGratingFilter (CartesianGratingFilterParams gRows gCols _ _ _ scales fs as) _) =
    (L.product . L.map L.length $ [scales, fs, as]) * gRows * gCols
  getFilterParameter = getCartesianGratingFilterParams
  {-# INLINE getFilterVectors #-}
  getFilterVectors (CartesianGratingFilter _ vecs) = vecs
  {-# INLINE changeSizeParameter #-}
  changeSizeParameter rows cols (CartesianGratingFilter (CartesianGratingFilterParams gRows gCols _ _ df scale freq angle) vecs) =
    CartesianGratingFilter
      (CartesianGratingFilterParams gRows gCols rows cols df scale freq angle)
      vecs

{-# INLINE cartesianGrating #-}

cartesianGrating :: Double -> Double -> Double -> Int -> Int -> Complex Double
cartesianGrating scale theta freq x y =
  (0 :+ gaussian2D scale x y) * exp (0 :+ freq * u)
  where
    c = cos theta
    s = sin theta
    u = fromIntegral x * c - fromIntegral y * s
