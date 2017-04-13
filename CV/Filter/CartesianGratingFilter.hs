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
  { getCartesianGratingFilterRows             :: !Int
  , getCartesianGratingFilterCols             :: !Int
  , getCartesianGratingFilterScale            :: ![Double]
  , getCartesianGratingFilterFreq             :: ![Double]
  , getCartesianGratingFilterAngle            :: ![Double]
  } deriving (Show)

instance NFData CartesianGratingFilterParams where
  rnf !_ = ()

data CartesianGratingFilter = CartesianGratingFilter
  { getCartesianGratingFilterParams :: CartesianGratingFilterParams
  , getCartesianGratingFilter :: V4SeparableFilter
  }

instance NFData CartesianGratingFilter where
  rnf !_ = ()

instance FilterExpansion CartesianGratingFilter where
  type FilterParameter CartesianGratingFilter = CartesianGratingFilterParams
  {-# INLINE makeFilter #-}
  makeFilter (CartesianGratingFilter params@(CartesianGratingFilterParams rows cols scales freqs angles) _) =
    CartesianGratingFilter params . V4CartesianSeparableFilter freqs $
    [ [ VU.fromListN
         (cols * rows)
         [ cartesianGrating scale angle freq (x - centerC) (y - centerR)
         | y <- [0 .. rows - 1]
         , x <- [0 .. cols - 1] ]
      | freq <- freqs ]
    | angle <- radAngles
    , scale <- scales ]
    where
      radAngles = L.map deg2Rad angles
      centerC = div cols 2
      centerR = div rows 2
  getFilterSize (CartesianGratingFilter (CartesianGratingFilterParams _ _ scales fs as) _) =
    L.product . L.map L.length $ [scales, fs, as]
  getFilterParameter = getCartesianGratingFilterParams
  {-# INLINE getFilterVectors #-}
  getFilterVectors (CartesianGratingFilter _ vecs) = vecs
  {-# INLINE changeSizeParameter #-}
  changeSizeParameter rows cols (CartesianGratingFilter (CartesianGratingFilterParams _ _ scale freq angle) vecs) =
    CartesianGratingFilter
      (CartesianGratingFilterParams rows cols scale freq angle)
      vecs

{-# INLINE cartesianGrating #-}

cartesianGrating :: Double -> Double -> Double -> Int -> Int -> Complex Double
cartesianGrating scale theta freq x y =
  (gaussian2D scale x y :+ 0) * exp (0 :+ pi * freq * u / scale)
  where
    c = cos theta
    s = sin theta
    u = fromIntegral x * c - fromIntegral y * s
