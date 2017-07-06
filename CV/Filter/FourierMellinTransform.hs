{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}
module CV.Filter.FourierMellinTransform
  ( module F
  , FourierMellinTransformParamsGrid(..)
  , FourierMellinTransformExpansion
  , FourierMellinTransformConvolution
  ) where

import           Control.Monad          as M
import           CV.Filter              as F
import           CV.Utility.Coordinates
import           CV.Utility.FFT
import           Data.Complex           as C
import           Data.List              as L
import           Data.Vector.Storable   as VS
import           Data.Vector.Unboxed    as VU

data FourierMellinTransformParamsGrid = FourierMellinTransformParamsGrid
  { getFourierMellinTransformGridRows        :: !Int
  , getFourierMellinTransformGridCols        :: !Int
  , getFourierMellinTransformGridRadialFreq  :: ![Double]
  , getFourierMellinTransformGridAngularFreq :: ![Int]
  } deriving (Show,Read)

type FourierMellinTransformExpansion = Filter FourierMellinTransformParamsGrid [[VU.Vector (Complex Double)]]
type FourierMellinTransformConvolution = Filter FourierMellinTransformParamsGrid [[VS.Vector (Complex Double)]]

instance FilterExpansion FourierMellinTransformExpansion where
  type FilterExpansionParameters FourierMellinTransformExpansion = FourierMellinTransformParamsGrid
  {-# INLINE makeFilterExpansion #-}
  makeFilterExpansion params@(FourierMellinTransformParamsGrid rows cols rfs afs) rCenter cCenter =
    Filter params $!
    [ [ VU.fromListN (rows * cols) $
       makeFilterExpansionList
         rows
         cols
         rCenter
         cCenter
         (fourierMellinTransform rf af)
      | af <- afs ]
    | rf <- rfs ]
  {-# INLINE getFilterExpansionNum #-}
  getFilterExpansionNum (Filter (FourierMellinTransformParamsGrid _ _ rfs afs) _) =
    L.length rfs * L.length afs
  {-# INLINE applyFilterExpansion #-}
  applyFilterExpansion (Filter _ filters) =
    L.concatMap (\x -> L.concatMap (L.map (VU.sum . VU.zipWith (*) x)) filters)
  {-# INLINE getFilterExpansionList #-}
  getFilterExpansionList = L.concat . getFilter


instance FilterConvolution FourierMellinTransformConvolution where
  type FilterConvolutionParameters FourierMellinTransformConvolution = FourierMellinTransformParamsGrid
  {-# INLINE makeFilterConvolution #-}
  makeFilterConvolution fftw params@(FourierMellinTransformParamsGrid rows cols rfs afs) filterType =
    Filter params <$!>
    M.mapM
      (\rf ->
          M.mapM
            (\af ->
                dft2d fftw rows cols .
                VS.fromListN (rows * cols) . conjugateFunc filterType $!
                makeFilterConvolutionList
                  rows
                  cols
                  (fourierMellinTransform rf af))
            afs)
      rfs
  {-# INLINE getFilterConvolutionNum #-}
  getFilterConvolutionNum (Filter (FourierMellinTransformParamsGrid _ _ rfs afs) _) =
    L.length rfs * L.length afs
  {-# INLINE applyFilterConvolution #-}
  applyFilterConvolution fftw (Filter (FourierMellinTransformParamsGrid rows cols _ _) filters) xs = do
    ys <- M.mapM (dft2d fftw rows cols) xs
    L.concat <$>
      M.mapM
        (\x ->
            L.concat <$>
            M.mapM (M.mapM (idft2d fftw rows cols . VS.zipWith (*) x)) filters)
        ys
  {-# INLINE getFilterConvolutionList #-}
  getFilterConvolutionList = L.concat . getFilter




{-# INLINE fourierMellinTransform #-}

fourierMellinTransform ::  Double -> Int -> Int -> Int -> Complex Double
fourierMellinTransform rf af x y
  | x == 0 && y == 0 = 0
  | otherwise =
    (r ** ((-0.5) :+ (-rf))) * exp (0 :+ (fromIntegral (-af) * theta))
  where
    r = sqrt . fromIntegral $ x ^ (2 :: Int) + y ^ (2 :: Int)
    theta = angleFunctionRad (fromIntegral x) (fromIntegral y)
