{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE DeriveGeneric #-}
module Application.GMM.MixtureModel where

import           Control.DeepSeq
import           Control.Monad   as M
import           Data.Binary
import           Data.Vector     as V
import           GHC.Generics
import           Prelude         as P
import           System.Random


newtype Model a =
  Model (Double,a)
  deriving (Generic)
  
instance Show a =>
         Show (Model a) where
  show (Model (x,y)) = "Weight: " P.++ show x P.++ " " P.++ show y

instance (Binary a) =>
         Binary (Model a) where
  put (Model x) = put x
  get =
    do x <- get
       return $ Model x

instance NFData a =>
         NFData (Model a) where
  rnf (Model (x, y)) = x `seq` y `seq` ()

data MixtureModel a =
  MixtureModel {numModel :: Int
               ,model :: V.Vector (Model a)}
  deriving (Generic)
  
instance NFData a =>
         NFData (MixtureModel a) where
  rnf (MixtureModel x y) = x `seq` y `seq` ()

instance (Binary a) =>
         Binary (MixtureModel a) where
  put (MixtureModel numModel' xs) =
    do put numModel'
       put $ V.toList xs
  get =
    do numModel' <- get
       xs <- get
       return (MixtureModel numModel'
                            (V.fromList xs))
                            
instance Show a =>
         Show (MixtureModel a) where
  show (MixtureModel n modelVec) =
    "MixtureModel " P.++ show n P.++ "\n" P.++
    V.foldl' (\s m -> s P.++ show m P.++ "\n") "" modelVec


initializeMixture :: Int -> V.Vector a -> IO (MixtureModel a)
initializeMixture numModel' !xs = do
  w <- M.replicateM numModel' (randomRIO (1, 1000))
  let !ws = P.sum w
      !normalizedW = V.fromList . P.map (/ ws) $ w
      !models = V.zipWith (curry Model) normalizedW xs
  return (MixtureModel numModel' models)
