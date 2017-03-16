{-# LANGUAGE TypeFamilies #-}

module CV.FilterExpansion where

import           Data.Complex
import           Data.Vector.Unboxed

class FilterExpansion a  where
  type FilterParameter a :: *
  makeFilter :: a -> a
  getFilterSize :: a -> Int
  getFilterParameter :: a -> FilterParameter a
  getFilterVectors :: a -> [Vector (Complex Double)]
  changeSizeParameter :: Int -> Int -> a -> a
