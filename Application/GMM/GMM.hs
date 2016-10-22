{-# LANGUAGE DeriveGeneric #-}
module Application.GMM.GMM where

import           Data.Binary
import           GHC.Generics

data Gaussian =
  Gaussian {numDims :: Int
           ,mu      :: [Double]
           ,sigma   :: [Double]}
  deriving (Show,Generic)

instance Binary Gaussian where
  put (Gaussian numDims' mu' sigma') =
    do put numDims'
       put mu'
       put sigma'
  get =
    do numDims' <- get
       mu' <- get
       sigma' <- get
       return (Gaussian numDims' mu' sigma')

data MixtureModel a =
  MixtureModel {numModel :: Int
               ,model    :: [(Double,a)]}
  deriving (Show, Generic)

instance (Binary a) =>
         Binary (MixtureModel a) where
  put (MixtureModel numModel' xs) =
    do put numModel'
       put xs
  get =
    do numModel' <- get
       xs <- get
       return (MixtureModel numModel' xs)
