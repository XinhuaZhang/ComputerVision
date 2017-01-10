module CV.Filter.GaussianFilter where

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
    

{-# INLINE gaussian2D' #-}
gaussian2D'
  :: (Floating a)
  => Int -> Int -> a -> Int -> Int -> a
gaussian2D' af rf sd i j
  | af == 0  =
    1 / ((2 * pi) * sd * sd) * exp (-r / (2 * (sd ^ (2 :: Int))))
  | otherwise =
    1 / ((2 * pi) * sd * sd) *
    exp (-(sqrt r - r0) ^ (2 :: Int) / (2 * (sd ^ (2 :: Int))))
  where
    r = fromIntegral (i * i + j * j)
    r0 = (8 * sd * (2 - exp (fromIntegral (-af) / 10))) / pi
