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
  type FilterExpansionParameters GaborFilterExpansion = GaborFilterParams
  {-# INLINE makeFilterExpansion #-}
  makeFilterExpansion params@(GaborFilterParams rows cols freqs scales oris) rCenter cCenter =
    Filter params $!
    [ [ [ VU.fromListN (rows * cols) $
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
    L.concatMap
      (\x -> L.concatMap (L.concatMap (L.map (VU.sum . VU.zipWith (*) x))) filters)
  {-# INLINE getFilterExpansionList #-}
  getFilterExpansionList = L.concatMap L.concat . getFilter

instance FilterConvolution GaborFilterConvolution where
  type FilterConvolutionParameters GaborFilterConvolution = GaborFilterParams
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
  {-# INLINE getFilterConvolutionNum #-}
  getFilterConvolutionNum (Filter (GaborFilterParams _ _ freqs scales oris) _) =
    L.length freqs * L.length scales * L.length oris
  {-# INLINE applyFilterConvolution #-}
  applyFilterConvolution fftw (Filter (GaborFilterParams rows cols _ _ _) filters) xs = do
    ys <- M.mapM (dft2d fftw rows cols) xs
    L.concat <$>
      M.mapM
        (\x ->
            L.concat <$>
            M.mapM
              (fmap L.concat .
               M.mapM (M.mapM (idft2d fftw rows cols . VS.zipWith (*) x)))
              filters)
        ys
  {-# INLINE getFilterConvolutionList #-}
  getFilterConvolutionList = L.concatMap L.concat . getFilter

{-# INLINE gabor #-}

gabor :: Double -> Double -> Double -> Int -> Int -> Complex Double
gabor freq scale ori x y =
  (exp ((-r) / (2 * scale ^ (2 :: Int))) :+ 0) *
  exp (0 :+ (-2) * pi * x' * freq)
  where
    r = fromIntegral $ x ^ (2 :: Int) + y ^ (2 :: Int)
    x' = fromIntegral x * cos ori + fromIntegral y * sin ori
