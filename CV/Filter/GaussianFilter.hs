{-# LANGUAGE FlexibleContexts #-}

module CV.Filter.GaussianFilter where

import           Control.Concurrent.MVar      (MVar)
import           Control.Monad
import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Parallel       as MP
import           Control.Monad.Trans.Resource
import           CV.Utility.FFT
import           CV.Utility.Parallel
import           CV.Utility.RepaArrayUtility
import           Data.Array.CArray            as CA
import           Data.Array.Repa              as R
import           Data.Complex                 as C
import           Data.Conduit
import           Data.Conduit.List            as CL
import           Data.List                    as L

data GaussianFilterParams = GaussianFilterParams
  { getGaussianFilterSigma :: !Double
  , getGaussianFilterRows  :: !Int
  , getGaussianFilterCols  :: !Int
  } deriving (Show)

data GaussianFilter a = GaussianFilter
  { getGaussianFilterParams :: GaussianFilterParams
  , getGaussianFilter       :: a
  }

instance Functor GaussianFilter where
  fmap f (GaussianFilter p a) = GaussianFilter p $ f a

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


{-# INLINE gaussian2D' #-}
gaussian2D'
  :: (Floating a, Ord a)
    => Int -> Int -> a -> Int -> Int -> a
gaussian2D' af rf sd i j -- =
  | sqrt r < sd = 1
  | otherwise = 0
  -- 1 / ((2 * pi) * sd * sd) *
  -- exp (-(sqrt r - r0) ^ (2 :: Int) / (2 * (sd ^ (2 :: Int))))
  where
    r = fromIntegral (i * i + j * j)
    r0 = ((1 - exp (-0.01 * fromIntegral (abs af) )) * 75 * sd) / pi


{-# INLINE gaussian2D'' #-}
gaussian2D''
  :: (Floating a, Ord a)
    => Int -> a -> Int -> Int -> a
gaussian2D'' freq sd i j -- =
  | r == 0 = 0
  | sqrt r < sd = 1 
  | otherwise = 1
  -- 1 / ((2 * pi) * sd * sd) *
  -- exp (-(sqrt r - r') ^ (2 :: Int) / (2 * (sd ^ (2 :: Int))))
  where
    r = fromIntegral (i * i + j * j)
    r' = ((1 - exp (-0.015 * fromIntegral (abs freq))) * 100 * sd) / pi

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
        r0 = (6 * sd * (2 - exp (fromIntegral (-af) / 10))) / pi

{-# INLINE gaussian2DDouble #-}

gaussian2DDouble :: Double -> Double -> Double -> Double
gaussian2DDouble sd i j =
  1 / ((2 * pi) * sd * sd) * exp (-r / (2 * (sd ^ (2 :: Int))))
  where
    r = i * i + j * j

makeFilter
  :: MVar ()
  -> GaussianFilterParams
  -> IO (GaussianFilter (R.Array U DIM2 (Complex Double)))
makeFilter lock params@(GaussianFilterParams sigma nRows nCols) =
  fmap (GaussianFilter params . computeS . twoDCArray2RArray) . dftN lock [0, 1] $ filterCArr
  where
    filterEleList = makeFilterList nRows nCols $ gaussian2D sigma
    filterCArr = listArray ((0, 0), (nRows - 1, nCols - 1)) . L.map (:+ 0) $ filterEleList

getFilter :: GaussianFilter a -> a
getFilter (GaussianFilter _ x) = x

applyFilterVariedSize
  :: (R.Source s Double)
  => MVar ()
  -> GaussianFilterParams
  -> R.Array s DIM3 Double
  -> IO (R.Array D DIM3 Double)
applyFilterVariedSize lock (GaussianFilterParams sigma _ _) arr = do
  dftFilterArr <-
    getGaussianFilter <$>
    makeFilter lock (GaussianFilterParams sigma nRows nCols)
  imageArr <- dftImageArr lock arr
  fmap (R.map magnitude . threeDCArray2RArray) .
    idftN lock [1, 2] . threeDRArray2CArray . traverse2 imageArr dftFilterArr const $
    \fImg fFilter idx@(Z :. _k :. j :. i) -> fImg idx * fFilter (Z :. j :. i)
  where
    (Z :. _ :. nRows :. nCols) = extent arr

applyFilterFixedSize
  :: (R.Source s Double)
  => MVar ()
  -> GaussianFilter (R.Array U DIM2 (Complex Double))
  -> R.Array s DIM3 Double
  -> IO (R.Array D DIM3 Double)
applyFilterFixedSize lock (GaussianFilter _params dftFilterArr) arr = do
  imageArr <- dftImageArr lock arr
  fmap (R.map magnitude . threeDCArray2RArray) .
    idftN lock [1, 2] . threeDRArray2CArray . traverse2 imageArr dftFilterArr const $
    \fImg fFilter idx@(Z :. _k :. j :. i) -> fImg idx * fFilter (Z :. j :. i)

gaussianVariedSizeConduit
  :: (R.Source s Double)
  => MVar ()
  -> ParallelParams
  -> GaussianFilterParams
  -> Conduit (R.Array s DIM3 Double) (ResourceT IO) (R.Array U DIM3 Double)
gaussianVariedSizeConduit lock parallelParams filterParams = do
  xs <- CL.take (batchSize parallelParams)
  unless
    (L.null xs)
    (do ys <-
          liftIO $
          MP.mapM
            (fmap computeUnboxedS . applyFilterVariedSize lock filterParams)
            xs
        sourceList ys
        gaussianVariedSizeConduit lock parallelParams filterParams)

{-# INLINE dftImageArr #-}

dftImageArr
  :: (R.Source s Double)
  => MVar () -> R.Array s DIM3 Double -> IO (R.Array D DIM3 (Complex Double))
dftImageArr lock arr =
  fmap threeDCArray2RArray .
  dftN lock [1, 2] .
  listArray ((0, 0, 0), (nFeature - 1, nRows - 1, nCols - 1)) .
  L.map (:+ 0) . R.toList $
  arr
  where
    (Z :. nFeature :. nRows :. nCols) = extent arr


{-# INLINE disk #-}
disk
  :: (Floating a, Ord a)
  => a -> Int -> Int -> a
disk sd i j
  | log r < sd = 1
  | otherwise = 0
  where
    r = sqrt $ fromIntegral (i * i + j * j)
