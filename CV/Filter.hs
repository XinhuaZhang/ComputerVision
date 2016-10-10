{-# LANGUAGE TypeFamilies #-}
module CV.Filter where

import           CV.Image

class Filter a  where
  type Input a :: *
  type Output a :: *
  displayFilter :: (Image img)
                => a -> img
  applyFilter :: a -> Input a -> Output a
