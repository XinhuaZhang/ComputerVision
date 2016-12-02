{-# LANGUAGE DeriveGeneric #-}
module Application.GMM.Gaussian where

import           Control.DeepSeq
import           Data.Binary
import           GHC.Generics
import           System.Random

data Gaussian = Gaussian
  { gaussianMu    :: Double
  , gaussianSigma :: Double
  } deriving (Show, Generic)

instance Binary Gaussian where
  put (Gaussian mu' sigma') = do
    put mu'
    put sigma'
  get = do
    mu' <- get
    sigma' <- get
    return (Gaussian mu' sigma')
    
instance NFData Gaussian where
  rnf (Gaussian x y) = x `seq` y `seq` ()

{-# INLINE gaussian #-}
gaussian :: Gaussian -> Double -> Double
gaussian (Gaussian mu' sigma') x =
  exp (-(x - mu') / 2 / sigma') / sqrt (2 * pi * sigma')

{-# INLINE randomGaussian #-}
randomGaussian :: (Double, Double) -> IO Gaussian
randomGaussian bound = do
  mu <- randomRIO bound
  sigma <- randomRIO bound
  return $! Gaussian mu sigma
