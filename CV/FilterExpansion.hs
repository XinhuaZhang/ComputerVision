{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}

module CV.FilterExpansion where

import           Control.DeepSeq
import           Data.Complex
import           Data.List            as L
import           Data.Vector.Storable as VS
import           Data.Vector.Unboxed  as VU

class FilterExpansion a  where
  type FilterParameter a :: *
  type FilterType a :: *
  makeFilter :: a -> (Int, Int) -> a
  getFilterSize :: a -> Int
  getFilterParameter :: a -> FilterParameter a
  getFilterVectors :: a -> FilterType a 
  changeSizeParameter :: Int -> Int -> a -> a



data V4SeparableFilter
  = V4PolarSeparableFilterAxis [Double]
                               [[VU.Vector (Complex Double)]]
  | V4PolarSeparableFilterGrid ([Double], [Double])
                               [[[VU.Vector (Complex Double)]]]
  | V4CartesianSeparableFilter [Double]
                               [[VU.Vector (Complex Double)]]
  | V4HyperbolicSeparableFilter [VU.Vector (Complex Double)]
  | FourierMellinTransform ([Double], [Double])
                           [[[VU.Vector (Complex Double)]]]
  | Null

instance NFData V4SeparableFilter where
  rnf !_ = ()



{-# INLINE grid1D #-}

grid1D :: Int -> Int -> [Int]
grid1D totalLen n = L.take n [len,2 * len .. n * len]
  where len = div totalLen (n + 1)

{-# INLINE grid2D #-}

grid2D :: (Int,Int) -> (Int,Int) -> [(Int,Int)]
grid2D (rows,cols) (nr,nc) = [(j,i)|j <- grid1D rows nr,i <- grid1D cols nc]
