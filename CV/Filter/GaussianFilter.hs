{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}

module CV.Filter.GaussianFilter
  ( module CV.Filter
  , module CV.Filter.GaussianFilter
  ) where

import           Control.DeepSeq
import           Control.Monad        as M
import           CV.Filter
import           CV.Utility.FFT
import           CV.Utility.Parallel
import           Data.Array.Repa      as R
import           Data.Complex         as C
import           Data.List            as L
import           Data.Vector.Storable as VS
import           Data.Vector.Unboxed  as VU


data GaussianFilterParams = GaussianFilterParams
  { getGaussianFilterSigma :: ![Double]
  , getGaussianFilterRows  :: !Int
  , getGaussianFilterCols  :: !Int
  } deriving (Show)

data GaussianFilter1DParams = GaussianFilter1DParams
  { getGaussianFilter1DSigma :: ![Double]
  , getGaussianFilter1DN     :: !Int
  } deriving (Show)

instance NFData GaussianFilterParams where
  rnf (GaussianFilterParams x y z) = x `seq` y `seq` z `seq` ()

type GaussianFilterConvolution = Filter GaussianFilterParams [VS.Vector (Complex Double)]
data GaussianFilterConvolution1D =
  GaussianFilterConvolution1D GaussianFilter1DParams
                              [VS.Vector (Complex Double)]


{-# INLINE gaussian1D #-}

gaussian1D
  :: (Floating a)
  => a -> a -> a
gaussian1D sd i =
  1 / (sqrt (2 * pi) * sd) *
  exp (-1 * (i ^ (2 :: Int)) / (2 * (sd ^ (2 :: Int))))

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
  | otherwise =
    1 / ((2 * pi) * sd * sd) *
    exp (-(sqrt r - r0) ^ (2 :: Int) / (2 * (sd ^ (2 :: Int))))
  where
    r = fromIntegral (i * i + j * j)
    r0 = 3 -- ((1 - exp (-0.01 * fromIntegral (abs af))) * 75 * sd) / pi


{-# INLINE gaussian2D'' #-}
gaussian2D''
  :: (Floating a, Ord a)
    => Int -> a -> Int -> Int -> a
gaussian2D'' freq sd i j -- =
  | r == 0 = 0 --  / ((2 * pi) * sd * sd)
  -- | r < sd = 1 / r
  | otherwise -- 1 / r
   =
    1 / ((2 * pi) * sd * sd) *
    exp (-(sqrt r - r') ^ (2 :: Int) / (2 * (sd ^ (2 :: Int))))
  where
    r = sqrt $ fromIntegral (i * i + j * j)
    r' = 0 -- ((1 - exp (-0.015 * fromIntegral (abs freq))) * 100 * sd) / pi

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

{-# INLINE disk #-}
disk
  :: (Floating a, Ord a)
  => a -> Int -> Int -> a
disk sd i j
  | log r < sd = 1
  | otherwise = 0
  where
    r = sqrt $ fromIntegral (i * i + j * j)

instance FilterConvolution GaussianFilterConvolution where
  type FilterConvolutionParameters GaussianFilterConvolution = GaussianFilterParams
  {-# INLINE getFilterConvolutionNum #-}
  getFilterConvolutionNum (Filter (GaussianFilterParams x _ _) _) = L.length x
  {-# INLINE getFilterConvolutionList #-}
  getFilterConvolutionList (Filter _ filters) = filters
  {-# INLINE makeFilterConvolution #-}
  makeFilterConvolution fftw params@(GaussianFilterParams scales rows cols) filterType =
    Filter params <$!>
    M.mapM
      (dft2d fftw rows cols .
       VS.fromListN (rows * cols) .
       conjugateFunc filterType .
       L.map (:+ 0) . makeFilterConvolutionList rows cols . gaussian2D)
      scales
  {-# INLINE applyFilterConvolution #-}
  applyFilterConvolution fftw (Filter (GaussianFilterParams _ rows cols) filters) xs = do
    ys <- M.mapM (dft2d fftw rows cols) xs
    L.concat <$>
      M.mapM (\x -> M.mapM (idft2d fftw rows cols . VS.zipWith (*) x) filters) ys


{-# INLINE makeFilterConvolution1DList #-}

makeFilterConvolution1DList
  :: (Num a)
  => Int -> (a -> a) -> [a]
makeFilterConvolution1DList n f =
  [ let x =
          if r < (n `div` 2)
            then r
            else r - n
    in f . fromIntegral $ x
  | r <- [0 .. n - 1] ]


instance FilterConvolution GaussianFilterConvolution1D where
  type FilterConvolutionParameters GaussianFilterConvolution1D = GaussianFilter1DParams
  {-# INLINE getFilterConvolutionNum #-}
  getFilterConvolutionNum (GaussianFilterConvolution1D (GaussianFilter1DParams x _) _) =
    L.length x
  {-# INLINE getFilterConvolutionList #-}
  getFilterConvolutionList (GaussianFilterConvolution1D _ filters) = filters
  {-# INLINE makeFilterConvolution #-}
  makeFilterConvolution fftw params@(GaussianFilter1DParams scales n) filterType =
    GaussianFilterConvolution1D params <$!>
    M.mapM
      (dft1d fftw .
       VS.fromList .
       conjugateFunc filterType .
       L.map (:+ 0) . makeFilterConvolution1DList n . gaussian1D)
      scales
  {-# INLINE applyFilterConvolution #-}
  applyFilterConvolution fftw (GaussianFilterConvolution1D (GaussianFilter1DParams _ _) filters) xs = do
    ys <- M.mapM (dft1d fftw) xs
    L.concat <$>
      M.mapM (\x -> M.mapM (idft1d fftw . VS.zipWith (*) x) filters) ys
