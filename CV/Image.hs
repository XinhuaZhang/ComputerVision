module CV.Image where

import           Data.Array.Repa as R

data Image a = Image
  { imageDepth   :: Int
  , imageContent :: a
  }

instance Functor Image where
  fmap f (Image d img) = Image d (f img)

type ImageRepa = Image (Array U DIM3 Double)

data ImageCoordinates
  = CartesianImage { getCartesianImageValueRange :: (Double, Double)
                   , getCartesianImage           :: Array U DIM3 Double}
  | PolarImage { getPolarImageR          :: Double
               , getPolarImageValueRange :: (Double, Double)
               , getPolarImage           :: Array U DIM3 Double}
  | LogpolarImage { getLogpolarR               :: Double
                  , getLogpolarImageValueRange :: (Double, Double)
                  , getLogpolarImage           :: Array U DIM3 Double}
