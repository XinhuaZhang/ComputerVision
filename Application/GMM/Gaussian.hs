{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE DeriveGeneric #-}
module Application.GMM.Gaussian where

import           Data.Binary
import           Data.Vector.Unboxed as VU
import           GHC.Generics
import           Prelude             as P

-- sigma is a diagonal matrix
data Gaussian =
  Gaussian {numDims :: Int
           ,mu      :: VU.Vector Double
           ,sigma   :: VU.Vector Double}
  deriving (Show,Generic)

instance Binary Gaussian where
  put (Gaussian numDims' mu' sigma') =
    do put numDims'
       put $ toList mu'
       put $ toList sigma'
  get =
    do numDims' <- get
       mu' <- get
       sigma' <- get
       return (Gaussian numDims'
                        (fromList mu')
                        (fromList sigma'))

{-!x = (2 * pi) ** (0.5 * (fromIntegral numDims'))-}
gaussian
  :: Gaussian -> VU.Vector Double -> Double
gaussian (Gaussian numDims' mu' sigma') xs
  | isNaN y = error "sigma is negative"
  | otherwise = result
  where !y = (VU.foldl' (\a b -> a * b ^ 2) 1 sigma') ** 0.5
        !z =
          (-0.5) *
          (VU.sum $
           VU.zipWith3 (\a b c -> ((a - b) / c) ^ 2)
                       xs
                       mu'
                       sigma')
        !result = (exp z) / y
