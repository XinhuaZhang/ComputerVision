{-# LANGUAGE DeriveGeneric #-}
module Application.GMM.Gaussian where

import           Control.DeepSeq
import           Data.Binary
import           GHC.Generics
import           System.Random

data Gaussian = Gaussian
  { gaussianMu    :: !Double
  , gaussianSigma :: !Double
  } deriving (Generic)
  
instance Show Gaussian where
  show (Gaussian mu sigma) = "Mu: " ++ show mu ++ " Sigma: " ++ show sigma

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
  exp (-((x - mu') ^ (2 :: Int)) / (2 * sigma')) / sqrt (2 * pi * sigma')

{-# INLINE randomGaussian #-}
randomGaussian
  :: ((Double,Double),(Double,Double)) -> IO Gaussian
randomGaussian (boundMu,boundSigma) =
  do mu <- randomRIO boundMu
     sigma <- randomRIO boundSigma
     return $! Gaussian mu sigma
