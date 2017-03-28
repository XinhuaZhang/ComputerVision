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

data CartesianSeparableFilterParams = CartesianSeparableFilterParams
  { getCartesianSeparableFilterGridRows         :: !Int
  , getCartesianSeparableFilterGridCols         :: !Int
  , getCartesianSeparableFilterRows             :: !Int
  , getCartesianSeparableFilterCols             :: !Int
  , getCartesianSeparableFilterDownsampleFactor :: !Int
  , getCartesianSeparableFilterScale            :: ![Double]
  , getCartesianSeparableFilterXFreq            :: ![Double]
  , getCartesianSeparableFilterYFreq            :: ![Double]
  } deriving (Show)

instance NFData CartesianSeparableFilterParams where
  rnf !_ = ()

data CartesianSeparableFilter = CartesianSeparableFilter
  { getCartesianSeparableFilterParams :: CartesianSeparableFilterParams
  , getCartesianSeparableFilter       :: [[VU.Vector (Complex Double)]]
  }

instance NFData CartesianSeparableFilter where
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

instance FilterExpansion CartesianSeparableFilter where
  type FilterParameter CartesianSeparableFilter = CartesianSeparableFilterParams
  {-# INLINE makeFilter #-}
  makeFilter (CartesianSeparableFilter params@(CartesianSeparableFilterParams gRows gCols rows cols downsampleFactor scales xFreqs yFreqs) _) =
    CartesianSeparableFilter params .
    L.map
      (\(centerR, centerC) ->
          [ VU.fromListN
             (newCols * newRows)
             [ cartesianSeparable scale xFreq yFreq (x - centerC) (y - centerR)
             | y <- [0 .. newRows - 1]
             , x <- [0 .. newCols - 1] ]
          | scale <- scales
          , xFreq <- xFreqs
          , yFreq <- yFreqs ] L.++
          [ let (xFreq', yFreq') =
                  if xFreq /= 0 && yFreq /= 0
                    then (-xFreq, yFreq)
                    else (-xFreq, -yFreq)
            in VU.fromListN
                 (newCols * newRows)
                 [ cartesianSeparable
                    scale
                    xFreq'
                    yFreq'
                    (x - centerC)
                    (y - centerR)
                 | y <- [0 .. newRows - 1]
                 , x <- [0 .. newCols - 1] ]
          | scale <- scales
          , xFreq <- xFreqs
          , yFreq <- yFreqs ]) $
    grid2D (newRows, newCols) (gRows, gCols)
    where
      newCols = div cols downsampleFactor
      newRows = div rows downsampleFactor
  getFilterSize (CartesianSeparableFilter (CartesianSeparableFilterParams gRows gCols _ _ _ scales xfs yfs) _) =
    (L.product . L.map L.length $ [scales, xfs, yfs]) * gRows * gCols
  getFilterParameter = getCartesianSeparableFilterParams
  {-# INLINE getFilterVectors #-}
  getFilterVectors (CartesianSeparableFilter _ vecs) = vecs
  {-# INLINE changeSizeParameter #-}
  changeSizeParameter rows cols (CartesianSeparableFilter (CartesianSeparableFilterParams gRows gCols _ _ df scale xFreq yFreq) vecs) =
    CartesianSeparableFilter
      (CartesianSeparableFilterParams gRows gCols rows cols df scale xFreq yFreq)
      vecs

{-# INLINE cartesianSeparable #-}

cartesianSeparable :: Double -> Double -> Double -> Int -> Int -> Complex Double
cartesianSeparable scale xFreq yFreq x y =
  (0 :+ gaussian2D scale x y) * exp (0 :+ xFreq * fromIntegral x) *
  exp (0 :+ yFreq * fromIntegral y)
