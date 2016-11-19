{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE TypeFamilies      #-}
--module CV.Filter.PolarSeparableFilterRepa where
module Main where

import           CV.Filter
import           CV.Filter.PolarSeparableFilter
import           CV.Image as IM
import           Data.Array.Repa                as R
import           Data.Array.Repa.Algorithms.FFT as R
import           Data.List                      as L
import           Data.Set                       as Set
import           Prelude                        as P
import Data.Array.IArray as IA
import Data.Array.CArray as CA
import Math.FFT
import Data.List as L
import Data.Complex as C

-- instance Filter (PolarSeparableFilter (Array U DIM3 Double)) where
--   type Input (PolarSeparableFilter (Array U DIM3 Double)) = Array U DIM3 Double
--   type Output (PolarSeparableFilter (Array U DIM3 Double)) = IO (Array U DIM3 Double)
--   type FilterParameter (PolarSeparableFilter (Array U DIM3 Double)) = PolarSeparableFilterParams
  

makeFilter
  :: PolarSeparableFilterParams -> PolarSeparableFilter (R.Array U DIM3 Double)
makeFilter params@(PolarSeparableFilterParams r scale rs as name) = undefined
  where
        --(PolarSeparableFilter params filterArr)
        size = 2 * r
        filterEleList =
          [pixelList (IM.makeFilter size
                                    size
                                    (getFilterFunc params s rf af) :: ComplexImage)
          |rf <- Set.toList rs
          ,af <- Set.toList as
          ,s <- Set.toList scale]
        nf = P.length filterEleList
        cArr =
          IA.listArray ((0,0,0),(nf - 1,size - 1,size - 1)) . L.concat $
          filterEleList :: CA.Array (Int,Int,Int) (C.Complex Double)
        
  
cArray2RArray :: 
