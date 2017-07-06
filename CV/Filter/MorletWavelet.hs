{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}
module CV.Filter.MorletWavelet
  ( module F
  , MorletWaveletParams(..)
  , MorletWaveletExpansion
  , MorletWaveletConvolution
  ) where

import           Control.Monad          as M
import           CV.Filter              as F
import           CV.Utility.Coordinates
import           CV.Utility.FFT
import           Data.Complex           as C
import           Data.List              as L
import           Data.Vector.Storable   as VS
import           Data.Vector.Unboxed    as VU


data MorletWaveletParams = MorletWaveletParams
  { morletWaveletRows          :: !Int
  , morletWaveletCols          :: !Int
  , morletWaveletFreq          :: !Double
  , morletWaveletGaussianScale :: !Double
  , morletWaveletOrientation   :: ![Double]
  , morletWaveletScale         :: ![Double]
  } deriving (Show, Read)

type MorletWaveletExpansion = Filter MorletWaveletParams [[VU.Vector (Complex Double)]]
type MorletWaveletConvolution = Filter MorletWaveletParams [[VS.Vector (Complex Double)]]

instance FilterExpansion MorletWaveletExpansion where
  type FilterExpansionParameters MorletWaveletExpansion = MorletWaveletParams
  {-# INLINE makeFilterExpansion #-}
  makeFilterExpansion params@(MorletWaveletParams rows cols freq gScale oris scales) rCenter cCenter =
    Filter params $!
    [ [ VU.fromListN (rows * cols) $
       makeFilterExpansionList
         rows
         cols
         rCenter
         cCenter
         (morletWavelet freq gScale ori a)
      | a <- scales ]
    | ori <- L.map deg2Rad oris ]
  {-# INLINE getFilterExpansionNum #-}
  getFilterExpansionNum (Filter (MorletWaveletParams _ _ _ _ oris scales) _) =
    L.length scales * L.length oris
  {-# INLINE applyFilterExpansion #-}
  applyFilterExpansion (Filter _ filters) =
    L.concatMap (\x -> L.concatMap (L.map (VU.sum . VU.zipWith (*) x)) filters)
  {-# INLINE getFilterExpansionList #-}
  getFilterExpansionList = L.concat . getFilter

instance FilterConvolution MorletWaveletConvolution where
  type FilterConvolutionParameters MorletWaveletConvolution = MorletWaveletParams
  {-# INLINE makeFilterConvolution #-}
  makeFilterConvolution fftw params@(MorletWaveletParams rows cols freq gScale oris scales) filterType =
    Filter params <$!>
    M.mapM
      ((\ori ->
           M.mapM
             (\a ->
                 dft2d fftw rows cols .
                 VS.fromListN (rows * cols) . conjugateFunc filterType $!
                 makeFilterConvolutionList
                   rows
                   cols
                   (morletWavelet freq gScale ori a))
             scales) .
       deg2Rad)
      oris
  {-# INLINE getFilterConvolutionNum #-}
  getFilterConvolutionNum (Filter (MorletWaveletParams _ _ _ _ oris scales) _) =
    L.length scales * L.length oris
  {-# INLINE applyFilterConvolution #-}
  applyFilterConvolution fftw (Filter (MorletWaveletParams rows cols _ _ _ _) filters) xs = do
    ys <- M.mapM (dft2d fftw rows cols) xs
    L.concat <$>
      M.mapM
        (\x ->
            L.concat <$>
            M.mapM (M.mapM (idft2d fftw rows cols . VS.zipWith (*) x)) filters)
        ys
  {-# INLINE getFilterConvolutionList #-}
  getFilterConvolutionList = L.concat . getFilter

{-# INLINE morletWavelet #-}

morletWavelet :: Double -> Double -> Double -> Double -> Int -> Int -> Complex Double
morletWavelet freq scale ori a x y =
  alpha * (exp (0 :+ (freq * x' / a)) - beta) * (exp (-r / (2 * s2) / (a * a)) :+ 0) /
  (sqrt a :+ 0)
  where
    r = fromIntegral $ x ^ (2 :: Int) + y ^ (2 :: Int)
    x' = fromIntegral x * cos ori + fromIntegral y * sin ori
    s2 = scale * scale
    alpha = 1 / scale :+ 0 --  / (2 * pi * s2) :+ 0  --2 * pi * s2 * exp ((freq * freq - 1) * s2 / 2) :+ 0
    beta = 0 -- (exp (-s2 / 2)) / (2 * pi * s2)
      :+ 0
