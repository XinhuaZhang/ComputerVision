module Application.GMM.GMM where

import           Application.GMM.Gaussian
import           Application.GMM.MixtureModel
import           Data.Binary
import           Data.Vector                  as V
import           Data.Vector.Unboxed          as VU
import           GHC.Generics
import           Prelude                      as P

type GMM = MixtureModel Gaussian

assignModelGMM
  :: GMM -> Model Gaussian -> VU.Vector Double -> Double
assignModelGMM (MixtureModel n modelVec) (Model (wk,mk)) xs =
  (wk * gaussian mk xs)
  where z =
          V.foldl' (\v (Model (wj,mj)) -> v + (wj * gaussian mj xs)) 0 modelVec
