{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
module CV.Filter.CartesianGratingFilter where

import           Control.DeepSeq
import           CV.Filter.GaussianFilter
import           CV.Utility.Coordinates
import           CV.FilterExpansion
import           Data.Complex             as C
import           Data.List                as L
import           Data.Vector.Unboxed      as VU

data CartesianGratingFilterParams = CartesianGratingFilterParams
  { getCartesianGratingFilterRows             :: !Int
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
  , getCartesianGratingFilter       :: [VU.Vector (Complex Double)]
  }

instance NFData CartesianGratingFilter where
  rnf !_ = ()

instance FilterExpansion CartesianGratingFilter where
  type FilterParameter CartesianGratingFilter = CartesianGratingFilterParams
  {-# INLINE makeFilter #-}
  makeFilter (CartesianGratingFilter params@(CartesianGratingFilterParams rows cols downsampleFactor scales freqs angles) _) =
    CartesianGratingFilter
      params
      [VU.fromListN
         (newCols * newRows)
         [cartesianGrating scale
                           angle
                           freq
                           (x - centerC)
                           (y - centerR)
         |y <- [0 .. newRows - 1]
         ,x <- [0 .. newCols - 1]]
      |angle <- radAngles
      ,scale <- scales
      ,freq <- freqs]
    where newCols = div cols downsampleFactor
          newRows = div rows downsampleFactor
          centerC = div newCols 2
          centerR = div newRows 2
          radAngles = L.map deg2Rad angles
  getFilterSize (CartesianGratingFilter (CartesianGratingFilterParams _ _ _ scales fs as) _) =
    L.product . L.map L.length $ [scales,fs,as]
  getFilterParameter = getCartesianGratingFilterParams
  {-# INLINE getFilterVectors #-}
  getFilterVectors (CartesianGratingFilter _ vecs) = vecs
  {-# INLINE changeSizeParameter #-}
  changeSizeParameter rows cols (CartesianGratingFilter (CartesianGratingFilterParams _ _ df scale freq angle) vecs) =
    CartesianGratingFilter (CartesianGratingFilterParams rows cols df scale freq angle)
                           vecs
  {-# INLINE makeFilterGrid #-}
  makeFilterGrid (nr,nc) (CartesianGratingFilter params@(CartesianGratingFilterParams rows cols downsampleFactor scales freqs angles) _) =
    CartesianGratingFilter params .
    L.concatMap
      (\(centerR,centerC) ->
         [VU.fromListN
            (newCols * newRows)
            [cartesianGrating scale
                              angle
                              freq
                              (x - centerC)
                              (y - centerR)
            |y <- [0 .. newRows - 1]
            ,x <- [0 .. newCols - 1]]
         |angle <- radAngles
         ,scale <- scales
         ,freq <- freqs]) $
    grid2D (newRows,newCols)
           (nr,nc)
    where newCols = div cols downsampleFactor
          newRows = div rows downsampleFactor
          radAngles = L.map deg2Rad angles
  getFilterSizeGrid (nr,nc) (CartesianGratingFilter (CartesianGratingFilterParams _ _ _ scales fs as) _) =
    (L.product . L.map L.length $ [scales,fs,as]) * nr * nc

{-# INLINE cartesianGrating #-}

cartesianGrating :: Double -> Double -> Double -> Int -> Int -> Complex Double
cartesianGrating scale theta freq x y =
  (0 :+ gaussian2D scale x y) * exp (0 :+ freq * u)
  where
    c = cos theta
    s = sin theta
    u = fromIntegral x * c - fromIntegral y * s
