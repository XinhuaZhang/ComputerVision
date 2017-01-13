{-# LANGUAGE DeriveGeneric #-}

module Application.MultiDimensionalGMM.Gaussian where

import           Control.DeepSeq
import           Control.Monad       as M
import           Data.Binary
import           Data.List           as L
import           Data.Vector.Unboxed as VU
import           GHC.Generics
import           System.Random

data Gaussian = Gaussian
  { gaussianMu     :: VU.Vector Double
  , gaussianSigma2 :: VU.Vector Double
  } deriving (Generic)

instance Show Gaussian where
  show (Gaussian mu sigma2) = "Mu: " L.++ show mu L.++ " Sigma: " L.++ show sigma2

instance Binary Gaussian where
  put (Gaussian mu' sigma2') = do
    put . VU.toList $ mu'
    put . VU.toList $ sigma2'
  get = do
    mu' <- get
    sigma2' <- get
    return (Gaussian (VU.fromList mu') (VU.fromList sigma2'))

instance NFData Gaussian where
  rnf (Gaussian x y) = x `seq` y `seq` ()

{-# INLINE gaussian #-}

gaussian :: Gaussian -> VU.Vector Double -> Double
gaussian (Gaussian mu' sigma2') xs =
  exp
    (-0.5 *
      VU.sum (VU.zipWith3 (\x m s2 -> (x - m) ^ (2 :: Int) / s2) xs mu' sigma2')) /
  sqrt (2 * pi * VU.product sigma2')

{-# INLINE randomGaussian #-}

randomGaussian :: Int -> ((Double, Double), (Double, Double)) -> IO Gaussian
randomGaussian nd (boundMu, boundSigma) = do
  mu <- M.replicateM nd $ randomRIO boundMu
  sigma <- M.replicateM nd $ randomRIO boundSigma
  return $! Gaussian (VU.fromList mu) (VU.fromList sigma)
