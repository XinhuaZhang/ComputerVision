module CV.Image where

import           Data.Array.Repa as R

data Image a = Image
  { imageDepth   :: Int
  , imageContent :: a
  }

instance Functor Image where
  fmap f (Image d img) = Image d (f img)

type ImageRepa = Image (Array U DIM3 Double)
