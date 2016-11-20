{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE DeriveGeneric     #-}

module CV.Feature.PolarSeparable where

import           Control.DeepSeq                    as DS
import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put
import           Data.Vector.Unboxed                as VU
import           GHC.Generics
import           Prelude                            as P

data PolarSeparableFeaturePoint =
  PolarSeparableFeaturePoint {x       :: Int
                             ,y       :: Int
                             ,feature :: VU.Vector Double}
  deriving (Show,Read,Generic)

instance DS.NFData PolarSeparableFeaturePoint where
  rnf !_ = ()

instance Binary PolarSeparableFeaturePoint where
  put (PolarSeparableFeaturePoint x y feature) =
    do put x
       put y
       put $ VU.toList feature
  get =
    do x <- get :: Get Int
       y <- get :: Get Int
       feature <- get :: Get [Double]
       return (PolarSeparableFeaturePoint x
                                          y
                                          (VU.fromList feature))

