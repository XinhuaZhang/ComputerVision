{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}

module Application.MultiDimensionalGMM.MixtureModel where

import           Control.DeepSeq
import           Control.Monad   as M
import           Data.Binary
import           Data.List       as L
import           GHC.Generics
import           System.Random

newtype Model a =
  Model (Double, a)
  deriving (Generic)
  
instance Show a =>
         Show (Model a) where
  show (Model (x, y)) = "Weight: " L.++ show x L.++ " " L.++ show y

instance (Binary a) =>
         Binary (Model a) where
  put (Model x) = put x
  get = do
    x <- get
    return $ Model x

instance NFData a =>
         NFData (Model a) where
  rnf (Model (x, y)) = x `seq` y `seq` ()

data MixtureModel a = MixtureModel
  { numModel :: Int
  , model    :: [Model a]
  } deriving (Generic)

instance NFData a =>
         NFData (MixtureModel a) where
  rnf (MixtureModel x y) = x `seq` y `seq` ()

instance (Binary a) =>
         Binary (MixtureModel a) where
  put (MixtureModel numModel' xs) = do
    put numModel'
    put xs
  get = do
    numModel' <- get
    xs <- get
    return (MixtureModel numModel' xs)

instance Show a =>
         Show (MixtureModel a) where
  show (MixtureModel n modelVec) =
    "MixtureModel " L.++ show n L.++ "\n" L.++
    L.foldl' (\s m -> s L.++ show m L.++ "\n") "" modelVec

initializeMixture :: Int -> [a] -> IO (MixtureModel a)
initializeMixture numModel' !xs = do
  w <- M.replicateM numModel' (randomRIO (1, 1000))
  let !ws = L.sum w
      !normalizedW = L.map (/ ws) w
      !models = L.zipWith (curry Model) normalizedW xs
  return (MixtureModel numModel' models)
