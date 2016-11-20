{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE TypeFamilies      #-}

module CV.Filter.PolarSeparableFilterRepa where

import           CV.Filter
import           CV.Filter.PolarSeparableFilter
import           CV.Image                       as IM
import           Data.Array.CArray              as CA
import           Data.Array.Repa                as R
import           Data.Complex                   as C
import           Data.List                      as L
import           Data.Set                       as Set
import           Foreign.Storable
import           Math.FFT
import           Prelude                        as P

-- The layout is nf x ny x nx
instance Filter (PolarSeparableFilter (R.Array U DIM3 (C.Complex Double))) where
  type Input (PolarSeparableFilter (R.Array U DIM3 (C.Complex Double))) = R.Array U DIM3 Double
  type Output (PolarSeparableFilter (R.Array U DIM3 (C.Complex Double))) = R.Array D DIM3 (C.Complex Double)
  type FilterParameter (PolarSeparableFilter (R.Array U DIM3 (C.Complex Double))) = PolarSeparableFilterParams
  makeFilter
    :: PolarSeparableFilterParams
    -> PolarSeparableFilter (R.Array U DIM3 (C.Complex Double))
  makeFilter params@(PolarSeparableFilterParams r scale rs as _name) =
    PolarSeparableFilter params filterArr
    where size' = 2 * r
          filterEleList =
            [pixelList (IM.makeFilter size'
                                      size'
                                      (getFilterFunc params s rf af) :: ComplexImage)
            |rf <- Set.toList rs
            ,af <- Set.toList as
            ,s <- Set.toList scale]
          nf = P.length filterEleList
          cArr =
            listArray ((0,0,0),(nf - 1,size' - 1,size' - 1)) . L.concat $
            filterEleList
          dftCArr = dftN [1,2] cArr
          filterArr = computeS $ threeDCArray2RArray dftCArr
  displayFilter :: PolarSeparableFilter (R.Array U DIM3 (C.Complex Double))
                -> ColorImage
  displayFilter = undefined
  applyFilter
    :: PolarSeparableFilter (R.Array U DIM3 (C.Complex Double))
    -> R.Array U DIM3 Double
    -> R.Array D DIM3 (C.Complex Double)
  applyFilter (PolarSeparableFilter _params filterArr) inputArr =
    threeDCArray2RArray . idftN [1,2] . threeDRArray2CArray $
    fromFunction
      (Z :. (n * nf) :. ny :. nx)
      (\(Z :. k :. j :. i) ->
         let k1 = div k nf
             k2 = mod k nf
         in (rArr R.! (Z :. k1 :. j :. i)) *
            (filterArr R.! (Z :. k2 :. j :. i)))
    where cArr = threeDRArray2CArray (R.map (\x -> x C.:+ 0) inputArr)
          dftCArr = dftN [1,2] cArr
          rArr = threeDCArray2RArray dftCArr
          (Z :. nf :. ny :. nx) = extent filterArr
          (Z :. n :. _ :. _) = extent rArr

twoDCArray2RArray
  :: (Num a
     ,Storable a)
  => CArray (Int,Int) a -> R.Array D DIM2 a
twoDCArray2RArray cArr =
  fromFunction (Z :. (ny' + 1) :. (nx' + 1))
               (\(Z :. j :. i) -> cArr CA.! (j,i))
  where ((_,_),(ny',nx')) = bounds cArr

threeDRArray2CArray
  :: (Num a
     ,Storable a
     ,Source s a)
  => R.Array s DIM3 a -> CArray (Int,Int,Int) a
threeDRArray2CArray rArr =
  listArray ((0,0,0),(nf - 1,ny - 1,nx - 1)) . R.toList $ rArr
  where (Z :. nf :. ny :. nx) = extent rArr

threeDCArray2RArray
  :: (Num a
     ,Storable a)
  => CArray (Int,Int,Int) a -> R.Array D DIM3 a
threeDCArray2RArray cArr =
  fromFunction (Z :. (nf' + 1) :. (ny' + 1) :. (nx' + 1))
               (\(Z :. k :. j :. i) -> cArr CA.! (k,j,i))
  where ((_,_,_),(nf',ny',nx')) = bounds cArr
