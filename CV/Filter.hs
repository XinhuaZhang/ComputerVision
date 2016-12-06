{-# LANGUAGE TypeFamilies #-}
module CV.Filter where

import           CV.Image

class Filter a  where
  type Input a :: *
  type Output a :: *
  type FilterParameter a :: *
  makeFilter :: FilterParameter a -> a
  displayFilter
    :: FilterParameter a -> ComplexImage
  applyFilterFixedSize :: a -> Input a -> Output a
  applyFilterVariedSize :: FilterParameter a -> Input a -> Output a
