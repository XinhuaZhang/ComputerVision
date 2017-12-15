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
  makeFilterConvolution plan params@(GaussianFilterParams scales rows cols) filterType = do
    let filterList =
          L.map
            (VS.fromList .
             conjugateFunc filterType .
             L.map (:+ 0) . makeFilterConvolutionList rows cols . gaussian2D)
            scales
        filterList1 =
          L.map
            (VS.fromList .
             conjugateFunc filterType .
             L.map (:+ 0) . makeFilterConvolutionList rows cols . gaussian2D)
            scales
    lock <- getFFTWLock
    (p1, vec) <- dft2dPlan lock plan rows cols . L.last $ filterList1
    (p2, _) <- idft2dPlan lock p1 rows cols vec
    filters <-
      Filter params <$!>
      dftExecuteBatch p2 (DFTPlanID DFT2D [rows, cols] []) filterList
    return (p2, filters)
  {-# INLINE applyFilterConvolution #-}
  applyFilterConvolution plan (Filter (GaussianFilterParams _ rows cols) filters) xs = do
    ys <- dftExecuteBatch plan (DFTPlanID DFT2D [rows, cols] []) xs
    dftExecuteBatch plan (DFTPlanID IDFT2D [rows, cols] []) .
      L.concatMap (\x -> L.map (VS.zipWith (*) x) filters) $
      ys


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
  makeFilterConvolution plan params@(GaussianFilter1DParams scales n) filterType = do
    let filterList =
          L.map
            (VS.fromList .
             conjugateFunc filterType .
             L.map (:+ 0) . makeFilterConvolution1DList n . gaussian1D)
            scales
        filterList1 =
          L.map
            (VS.fromList .
             conjugateFunc filterType .
             L.map (:+ 0) . makeFilterConvolution1DList n . gaussian1D)
            scales
    lock <- getFFTWLock
    (p1, vec) <- dft1dGPlan lock plan [n] [0] . L.last $ filterList1
    (p2, _) <- idft1dGPlan lock p1 [n] [0] vec
    filters <-
      GaussianFilterConvolution1D params <$!>
      dftExecuteBatch p2 (DFTPlanID DFT1DG [n] [0]) filterList
    return (p2, filters)
  {-# INLINE applyFilterConvolution #-}
  applyFilterConvolution plan (GaussianFilterConvolution1D (GaussianFilter1DParams _ n) filters) xs = do
    ys <- dftExecuteBatch plan (DFTPlanID DFT1DG [n] [0]) xs
    dftExecuteBatch plan (DFTPlanID IDFT1DG [n] [0]) .
      L.concatMap (\x -> L.map (VS.zipWith (*) x) filters) $ys
