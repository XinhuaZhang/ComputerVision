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

gaussian
  :: Gaussian -> VU.Vector Double -> Double
gaussian (Gaussian numDims' mu' sigma') xs = (exp z) / (x * y)
  where x = (2 * pi) ** (-0.5 * (fromIntegral numDims'))
        y = (VU.foldl1' (*) sigma') ** 0.5
        z =
          (-0.5) *
          (VU.sum $
           VU.zipWith3 (\a b c -> (a - b) ^ 2 / c)
                       xs
                       mu'
                       sigma')
