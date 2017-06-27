{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}
module CV.Filter.Gabor
  ( module F
  , GaborFilterParams(..)
  , GaborFilterExpansion
  , GaborFilterConvolution
  ) where

import           Control.Monad          as M
import           CV.Filter              as F
import           CV.Utility.Coordinates
import           CV.Utility.FFT
import           Data.Complex           as C
import           Data.List              as L
import           Data.Vector.Storable   as VS
import           Data.Vector.Unboxed    as VU


data GaborFilterParams = GaborFilterParams
  { gaborFilterRows        :: !Int
  , gaborFilterCols        :: !Int
  , gaborFilterFreq        :: ![Double]
  , gaborFilterScale       :: ![Double]
  , gaborFilterOrientation :: ![Double]
  } deriving (Show,Read)

type GaborFilterExpansion = Filter GaborFilterParams [[[VU.Vector (Complex Double)]]]
type GaborFilterConvolution = Filter GaborFilterParams [[[VS.Vector (Complex Double)]]]

instance FilterExpansion GaborFilterExpansion where
  type FilterExpansionInputType GaborFilterExpansion = [VU.Vector (Complex Double)]
  type FilterExpansionOutputType GaborFilterExpansion = [[[[Complex Double]]]]
  type FilterExpansionParameters GaborFilterExpansion = GaborFilterParams
  type FilterExpansionFilterType GaborFilterExpansion = VU.Vector (Complex Double)
  {-# INLINE makeFilterExpansion #-}
  makeFilterExpansion params@(GaborFilterParams rows cols freqs scales oris) rCenter cCenter =
    Filter params $!
    [ [ [ VU.fromListN (rows * cols) $!
         makeFilterExpansionList
           rows
           cols
           rCenter
           cCenter
           (gabor freq scale ori)
        | freq <- freqs ]
      | ori <- L.map deg2Rad oris ]
    | scale <- scales ]
  {-# INLINE getFilterExpansionNum #-}
  getFilterExpansionNum (Filter (GaborFilterParams _ _ freqs scales oris) _) =
    L.length freqs * L.length scales * L.length oris
  {-# INLINE applyFilterExpansion #-}
  applyFilterExpansion (Filter _ filters) =
    L.map (\x -> L.map (L.map (L.map (VU.sum . VU.zipWith (*) x))) filters)
  {-# INLINE getFilterExpansionList #-}
  getFilterExpansionList = L.concatMap L.concat . getFilter

instance FilterConvolution GaborFilterConvolution where
  type FilterConvolutionInputType GaborFilterConvolution = [VS.Vector (Complex Double)]
  type FilterConvolutionOutputType GaborFilterConvolution = [[[[VS.Vector (Complex Double)]]]]
  type FilterConvolutionParameters GaborFilterConvolution = GaborFilterParams
  type FilterConvolutionFilterType GaborFilterConvolution = VS.Vector (Complex Double)
  {-# INLINE makeFilterConvolution #-}
  makeFilterConvolution fftw params@(GaborFilterParams rows cols freqs scales oris) filterType =
    Filter params <$!>
    M.mapM
      (\scale ->
          M.mapM
            ((\ori ->
                 M.mapM
                   (\freq ->
                       dft2d fftw rows cols .
                       VS.fromListN (rows * cols) . conjugateFunc filterType $!
                       makeFilterConvolutionList
                         rows
                         cols
                         (gabor freq scale ori))
                   freqs) .
             deg2Rad)
            oris)
      scales
    where
      conjugateFunc x =
        case x of
          Normal -> id
          Conjugate -> L.map conjugate
  {-# INLINE getFilterConvolutionNum #-}
  getFilterConvolutionNum (Filter (GaborFilterParams _ _ freqs scales oris) _) =
    L.length freqs * L.length scales * L.length oris
  {-# INLINE applyFilterConvolution #-}
  applyFilterConvolution fftw (Filter (GaborFilterParams rows cols _ _ _) filters) xs = do
    ys <- M.mapM (dft2d fftw rows cols) xs
    M.mapM
      (\x ->
          M.mapM (M.mapM (M.mapM (idft2d fftw rows cols . VS.zipWith (*) x))) filters)
      ys
  {-# INLINE getFilterConvolutionList #-}
  getFilterConvolutionList = L.concatMap L.concat . getFilter

{-# INLINE gabor #-}

gabor :: Double -> Double -> Double -> Int -> Int -> Complex Double
gabor freq scale ori x y =
  ((exp ((-r) / (2 * scale ^ (2 :: Int)))) :+ 0) * exp (0 :+ (-2) * pi * x' * freq)
  where
    r = fromIntegral $ x ^ (2 :: Int) + y ^ (2 :: Int)
    x' = fromIntegral x * cos ori + fromIntegral y * sin ori
