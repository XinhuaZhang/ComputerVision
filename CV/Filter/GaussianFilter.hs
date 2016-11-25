module CV.Filter.GaussianFilter where

import           CV.Filter
import           CV.Image

data GaussianFilterParams = GaussianFilterParams
  { getGaussianFilterRadius :: Int
  , getGaussianFilterSigma  :: Double
  } deriving (Show)

data GaussianFilter a = GaussianFilter
  { getGaussianFilterParams :: GaussianFilterParams
  , getGaussianFilter :: a
  }
  
{-# INLINE gaussian1D #-}  
gaussian1D
  :: (Floating a)
  => a -> Int -> a
gaussian1D sd i =
  1 / ((sqrt (2 * pi)) * sd) *
  exp (-1 * (fromIntegral i ^ 2) / (2 * (sd ^ 2)))

{-# INLINE gaussian2D #-}
gaussian2D
  :: (Floating a)
  => a -> Int -> Int -> a
gaussian2D sd i j = 1 / ((2 * pi) * sd * sd) * exp (-r / (2 * (sd ^ 2)))
  where
    r = fromIntegral (i * i + j * j)
