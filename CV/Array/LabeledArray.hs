{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveGeneric #-}
module CV.Array.LabeledArray where

import           Data.Array.Repa     as R
import           Data.Binary
import           Data.Vector.Unboxed as VU
import           GHC.Generics

data LabeledArray sh e =
  LabeledArray !Int
               (Array U sh e)
  deriving (Show,Generic)

instance (Binary e
         ,Unbox e
         ,Shape sh) =>
         Binary (LabeledArray sh e) where
  put (LabeledArray label arr) =
    do put label
       put . listOfShape . extent $ arr
       put . R.toList $ arr
  get =
    do label' <- get
       shList <- get
       elemList <- get
       return $!
         LabeledArray
           label'
           (fromListUnboxed (shapeOfList shList)
                            elemList)
