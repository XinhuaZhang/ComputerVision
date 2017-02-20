{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

module CV.Filter.GaussianFilter where

import           Control.Monad
import           Control.Monad.Trans.Resource
import           CV.Utility.Parallel
import           CV.Utility.RepaArrayUtility
import           Data.Array.CArray            as CA
import           Data.Array.Repa              as R
import           Data.Complex                 as C
import           Data.Conduit
import           Data.Conduit.List            as CL
import           Data.List                    as L
import           Math.FFT

data GaussianFilterParams = GaussianFilterParams
  { getGaussianFilterSigma :: !Double
  , getGaussianFilterSize  :: (Int, Int)
  } deriving (Show)

data GaussianFilter a = GaussianFilter
  { getGaussianFilterParams :: GaussianFilterParams
  , getGaussianFilter       :: a
  }

{-# INLINE gaussian1D #-}

gaussian1D
  :: (Floating a)
  => a -> Int -> a
gaussian1D sd i =
  1 / (sqrt (2 * pi) * sd) *
  exp (-1 * (fromIntegral i ^ (2 :: Int)) / (2 * (sd ^ (2 :: Int))))

{-# INLINE gaussian2D #-}

gaussian2D
  :: (Floating a)
  => a -> Int -> Int -> a
gaussian2D sd i j =
  1 / ((2 * pi) * sd * sd) * exp (-r / (2 * (sd ^ (2 :: Int))))
  where
    r = fromIntegral (i * i + j * j)

{-# INLINE gaussian2DRing #-}

gaussian2DRing
  :: (Floating a)
  => Int -> Int -> a -> Int -> Int -> a
gaussian2DRing af rf sd i j
  | af == 0 = 1 / ((2 * pi) * sd * sd) * exp (-r / (2 * (sd ^ (2 :: Int))))
  | otherwise =
    1 / ((2 * pi) * sd * sd) *
    exp (-(sqrt r - r0) ^ (2 :: Int) / (2 * (sd ^ (2 :: Int))))
  where r = fromIntegral (i * i + j * j)
        r0 = (10 * sd * (2 - exp (fromIntegral (-af) / 10))) / pi

{-# INLINE gaussian2DDouble #-}

gaussian2DDouble :: Double -> Double -> Double -> Double
gaussian2DDouble sd i j =
  1 / ((2 * pi) * sd * sd) * exp (-r / (2 * (sd ^ (2 :: Int))))
  where
    r = i * i + j * j

makeFilter :: GaussianFilterParams
           -> GaussianFilter (R.Array U DIM2 (Complex Double))
makeFilter params@(GaussianFilterParams sigma (nRows, nCols)) =
  deepSeqArray dftFilterArr $! GaussianFilter params dftFilterArr
  where
    !filterEleList = makeFilterList nRows nCols $ gaussian2D sigma
    !filterCArr = listArray ((0, 0), (nRows - 1, nCols - 1)) . L.map (:+ 0) $ filterEleList
    !dftFilterArr = computeS . twoDCArray2RArray $ dftN [0, 1] filterCArr

applyFilterVariedSize
  :: (R.Source s Double)
  => GaussianFilterParams -> R.Array s DIM3 Double -> R.Array D DIM3 Double
applyFilterVariedSize (GaussianFilterParams sigma _) arr =
  R.map magnitude .
  threeDCArray2RArray .
  idftN [1, 2] . threeDRArray2CArray . traverse2 imageArr dftFilterArr const $
  \fImg fFilter idx@(Z :. _k :. j :. i) -> fImg idx * fFilter (Z :. j :. i)
  where
    !(Z :. _ :. nRows :. nCols) = extent arr
    !imageArr = dftImageArr arr
    !dftFilterArr =
      getGaussianFilter $ makeFilter (GaussianFilterParams sigma (nRows, nCols))

applyFilterFixedSize
  :: (R.Source s Double)
  => GaussianFilter (R.Array U DIM2 (Complex Double))
  -> R.Array s DIM3 Double
  -> R.Array D DIM3 Double
applyFilterFixedSize (GaussianFilter _params dftFilterArr) arr =
  R.map magnitude .
  threeDCArray2RArray .
  idftN [1, 2] .
  threeDRArray2CArray . traverse2 (dftImageArr arr) dftFilterArr const $
  \fImg fFilter idx@(Z :. _k :. j :. i) -> fImg idx * fFilter (Z :. j :. i)


gaussianVariedSizeConduit
  :: (R.Source s Double)
  => ParallelParams
  -> GaussianFilterParams
  -> Conduit (R.Array s DIM3 Double) (ResourceT IO) (R.Array U DIM3 Double)
gaussianVariedSizeConduit parallelParams filterParams = do
  xs <- CL.take (batchSize parallelParams)
  unless
    (L.null xs)
    (do let !ys =
              parMapChunk
                parallelParams
                rseq
                (\y ->
                    let !arr =
                          computeUnboxedS $ applyFilterVariedSize filterParams y
                    in deepSeqArray arr arr)
                xs
        sourceList ys
        gaussianVariedSizeConduit parallelParams filterParams)

{-# INLINE dftImageArr #-}

dftImageArr
  :: (R.Source s Double)
  => R.Array s DIM3 Double -> R.Array D DIM3 (Complex Double)
dftImageArr arr = threeDCArray2RArray dftCArr
  where
    !(Z :. nFeature :. nRows :. nCols) = extent arr
    !cArr =
      listArray ((0, 0, 0), (nFeature - 1, nRows - 1, nCols - 1)) .
      L.map (:+ 0) . R.toList $
      arr
    !dftCArr = dftN [1, 2] cArr
