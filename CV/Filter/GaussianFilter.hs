{-# LANGUAGE FlexibleContexts #-}

module CV.Filter.GaussianFilter where

import           Control.Concurrent.MVar      (MVar)
import           Control.Monad                as M
import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Trans.Resource
import           CV.Utility.FFT
import           CV.Utility.Parallel
import           Data.Array.Repa              as R
import           Data.Complex                 as C
import           Data.Conduit
import           Data.Conduit.List            as CL
import           Data.List                    as L
import           Data.Vector.Unboxed          as VU
import           Data.Vector.Storable         as VS
import CV.FilterConvolution


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
  | r == 0 = 1
  -- | r < sd = 1 / r
  | otherwise = 1 / r
  -- 1 / ((2 * pi) * sd * sd) *
  -- exp (-(sqrt r - r') ^ (2 :: Int) / (2 * (sd ^ (2 :: Int))))
  where
    r = sqrt $ fromIntegral (i * i + j * j)
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
  :: FFTW
  -> GaussianFilterParams
  -> IO (GaussianFilter (VS.Vector (Complex Double)))
makeFilter fftw params@(GaussianFilterParams sigma nRows nCols) =
  fmap (GaussianFilter params) . dft2d fftw nRows nCols $ filterCArr
  where
    filterEleList = makeFilterList nRows nCols $ gaussian2D sigma
    filterCArr = VS.fromListN (nRows * nCols) . L.map (:+ 0) $ filterEleList

-- getFilter :: GaussianFilter a -> a
-- getFilter (GaussianFilter _ x) = x

applyFilterVariedSize
  :: (R.Source s Double)
  => FFTW
  -> GaussianFilterParams
  -> R.Array s DIM3 Double
  -> IO (R.Array U DIM3 Double)
applyFilterVariedSize fftw (GaussianFilterParams sigma _ _) arr = do
  dftFilterArr <-
    getGaussianFilter <$>
    makeFilter fftw (GaussianFilterParams sigma nRows nCols)
  imageArr <-
    dftImageArr fftw (nRows, nCols) .
    L.map (\i -> toUnboxed . computeS . R.slice arr $ (Z :. i :. All :. All)) $
    [0 .. nf - 1]
  fromUnboxed (extent arr) . VU.map magnitude . VS.convert . VS.concat <$>
    M.mapM (idft2d fftw nRows nCols . VS.zipWith (*) dftFilterArr) imageArr
  where
    (Z :. nf :. nRows :. nCols) = extent arr

applyFilterFixedSize
  :: (R.Source s Double)
  => FFTW
  -> GaussianFilter (VS.Vector (Complex Double))
  -> R.Array s DIM3 Double
  -> IO (R.Array U DIM3 Double)
applyFilterFixedSize fftw (GaussianFilter _params dftFilterArr) arr = do
  imageArr <-
    dftImageArr fftw (nRows, nCols) .
    L.map (\i -> toUnboxed . computeS . R.slice arr $ (Z :. i :. All :. All)) $
    [0 .. nf - 1]
  fromUnboxed (extent arr) . VU.map magnitude . VS.convert . VS.concat <$>
    M.mapM (idft2d fftw nRows nCols . VS.zipWith (*) dftFilterArr) imageArr
  where
    (Z :. nf :. nRows :. nCols) = extent arr

gaussianVariedSizeConduit
  :: (R.Source s Double)
  => FFTW
  -> ParallelParams
  -> GaussianFilterParams
  -> Conduit (R.Array s DIM3 Double) (ResourceT IO) (R.Array U DIM3 Double)
gaussianVariedSizeConduit fftw parallelParams filterParams = do
  xs <- CL.take (batchSize parallelParams)
  unless
    (L.null xs)
    (do ys <- liftIO $ M.mapM (applyFilterVariedSize fftw filterParams) xs
        sourceList ys
        gaussianVariedSizeConduit fftw parallelParams filterParams)

{-# INLINE dftImageArr #-}

dftImageArr :: FFTW
            -> (Int, Int)
            -> [VU.Vector Double]
            -> IO [VS.Vector (Complex Double)]
dftImageArr fftw (nRows, nCols) =
  M.mapM (dft2d fftw nRows nCols . VU.convert . VU.map (:+ 0)) 



{-# INLINE disk #-}
disk
  :: (Floating a, Ord a)
  => a -> Int -> Int -> a
disk sd i j
  | log r < sd = 1
  | otherwise = 0
  where
    r = sqrt $ fromIntegral (i * i + j * j)
