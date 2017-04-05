{-# LANGUAGE TypeFamilies #-}

module CV.FilterExpansion where

import           Data.Complex
import           Data.List           as L
import           Data.Vector.Unboxed

class FilterExpansion a  where
  type FilterParameter a :: *
  type FilterVectors a :: *
  makeFilter :: a -> a
  getFilterSize :: a -> Int
  getFilterParameter :: a -> FilterParameter a
  getFilterVectors :: a -> FilterVectors a
  changeSizeParameter :: Int -> Int -> a -> a


{-# INLINE grid1D #-}

grid1D :: Int -> Int -> [Int]
grid1D totalLen n = L.take n [len,2 * len .. n * len]
  where len = div totalLen (n + 1)
  
{-# INLINE grid2D #-}

grid2D :: (Int,Int) -> (Int,Int) -> [(Int,Int)]
grid2D (rows,cols) (nr,nc) = [(j,i)|j <- grid1D rows nr,i <- grid1D cols nc]
