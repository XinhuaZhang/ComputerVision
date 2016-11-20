{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE TypeFamilies      #-}
module CV.Filter.PolarSeparableFilterAcc where

import           Control.DeepSeq
import           Control.Monad.IO.Class                (liftIO)
import           Control.Parallel
import           CV.CUDA.ArrayUtil
import           CV.CUDA.Context
import           CV.CUDA.FFT
import           CV.Filter
import           CV.Filter.GaussianFilter
import           CV.Filter.PolarSeparableFilter
import           CV.Image                              as IM
import           CV.Utility.Coordinates
import           CV.Utility.Parallel
import           Data.Array.Accelerate                 as A
import           Data.Array.Accelerate.Data.Complex    as A
import           Data.Array.Accelerate.Math.DFT.Centre as A
import           Data.Array.Accelerate.Math.FFT        as A
import           Data.Array.Unboxed                    as AU
import           Data.Complex                          as C
import           Data.Conduit
import           Data.Conduit.List                     as CL
import           Data.List                             as L
import           Data.Set                              as Set
import           GHC.Float
import           Prelude                               as P


instance Filter (PolarSeparableFilter (Acc (A.Array DIM3 (A.Complex Double)))) where
  type Input (PolarSeparableFilter (Acc (A.Array DIM3 (A.Complex Double)))) = Acc (A.Array DIM2 Double)
  type Output (PolarSeparableFilter (Acc (A.Array DIM3 (A.Complex Double)))) = Acc (A.Array DIM3 (A.Complex Double))
  type FilterParameter (PolarSeparableFilter (Acc (A.Array DIM3 (A.Complex Double)))) = PolarSeparableFilterParams
  {- Default format HWD -}
  makeFilter
    :: PolarSeparableFilterParams
    -> (PolarSeparableFilter (Acc (A.Array DIM3 (A.Complex Double))))
  makeFilter params@(PolarSeparableFilterParams r scale rs as name) =
    (PolarSeparableFilter params filterArr)
    where
      size = 2 * r
      filterEleList =
        [ pixelList
           (IM.makeFilter size size (getFilterFunc params s rf af) :: ComplexImage)
        | rf <- Set.toList rs
        , af <- Set.toList as
        , s <- Set.toList scale ]
      len = P.length filterEleList
      filterArr =
        (centre25D HWD >-> fft25D' A.Forward size size len HWD) .
        use . A.fromList (Z :. size :. size :. len) . P.concat . L.transpose $
         filterEleList
  displayFilter (PolarSeparableFilter _ imgAcc) = undefined
  applyFilter
    :: PolarSeparableFilter (Acc (A.Array DIM3 (A.Complex Double)))
    -> Acc (A.Array DIM2 Double)
    -> Acc (A.Array DIM3 (A.Complex Double))
  applyFilter (PolarSeparableFilter params@PolarSeparableFilterParams {getRadius = r} filterArr) imgAcc =
    fft25D' A.Inverse size size len HWD $ A.zipWith (*) imgArr filterArr
    where
      size = 2 * r
      len = getFilterNum params
      imgArr =
        A.replicate (lift (Z :. All :. All :. len)) .
        A.fft2D' A.Forward size size . centre2D . A.map (\x -> lift $ x C.:+ 0) $
        imgAcc

instance Filter (PolarSeparableFilter (Acc (A.Array DIM3 (A.Complex Float)))) where
  type Input (PolarSeparableFilter (Acc (A.Array DIM3 (A.Complex Float)))) = Acc (A.Array DIM2 Float)
  type Output (PolarSeparableFilter (Acc (A.Array DIM3 (A.Complex Float)))) = Acc (A.Array DIM3 (A.Complex Float))
  type FilterParameter (PolarSeparableFilter (Acc (A.Array DIM3 (A.Complex Float)))) = PolarSeparableFilterParams
  {- Default format HWD -}
  makeFilter
    :: PolarSeparableFilterParams
    -> (PolarSeparableFilter (Acc (A.Array DIM3 (A.Complex Float))))
  makeFilter params@(PolarSeparableFilterParams r scale rs as name) =
    (PolarSeparableFilter params filterArr)
    where
      size = 2 * r
      filterEleList =
        [ pixelList
           (IM.makeFilter size size (getFilterFunc params s rf af) :: ComplexImage)
        | rf <- Set.toList rs
        , af <- Set.toList as
        , s <- Set.toList scale ]
      len = P.length filterEleList
      filterArr =
        (centre25D HWD >-> fft25D' A.Forward size size len HWD) .
        use .
        A.fromList (Z :. size :. size :. len) .
        P.map (\(x C.:+ y) -> (double2Float x C.:+ double2Float y)) .
        P.concat . L.transpose $
        filterEleList
  displayFilter (PolarSeparableFilter _ imgAcc) = undefined
  applyFilter
    :: PolarSeparableFilter (Acc (A.Array DIM3 (A.Complex Float)))
    -> Acc (A.Array DIM2 Float)
    -> Acc (A.Array DIM3 (A.Complex Float))
  applyFilter (PolarSeparableFilter params@PolarSeparableFilterParams {getRadius = r} filterArr) imgAcc =
    fft25D' A.Inverse size size len HWD $ A.zipWith (*) imgArr filterArr
    where
      size = 2 * r
      len = getFilterNum params
      imgArr =
        A.replicate (lift (Z :. All :. All :. len)) .
        A.fft2D' A.Forward size size . centre2D . A.map (\x -> lift $ x C.:+ 0) $
        imgAcc
