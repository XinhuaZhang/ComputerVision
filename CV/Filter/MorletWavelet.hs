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
    [ [ let beta =
              (L.sum . makeFilterExpansionListPeriod rows cols rCenter cCenter $
               betaFunction freq gScale ori a) /
              (2 * pi * gScale ^ (2 :: Int) :+ 0)
            alpha =
              (sqrt a :+ 0) /
              (sqrt . L.sum . makeFilterExpansionListPeriod rows cols rCenter cCenter $
               alphaFunction freq gScale ori a beta)
        in VU.fromListN (rows * cols) $
           makeFilterExpansionListPeriod
             rows
             cols
             rCenter
             cCenter
             (morletWavelet freq gScale ori a alpha beta)
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
  makeFilterConvolution plan params@(MorletWaveletParams rows cols freq gScale oris' scales) filterType = do
    let rCenter = div rows 2
        cCenter = div cols 2
        oris = L.map deg2Rad oris'
        filterList =
          L.map
            (\ori ->
               L.map
                 (\a ->
                    let beta =
                          (L.sum .
                           makeFilterExpansionList rows cols rCenter cCenter $
                           betaFunction freq gScale ori a) /
                          (2 * pi * gScale ^ (2 :: Int) :+ 0)
                        alpha =
                          (sqrt a :+ 0) /
                          (sqrt .
                           L.sum .
                           makeFilterExpansionList rows cols rCenter cCenter $
                           alphaFunction freq gScale ori a beta)
                    in VS.fromList . conjugateFunc filterType $!
                       makeFilterConvolutionList
                         rows
                         cols
                         (morletWavelet freq gScale ori a alpha beta))
                 scales)
            oris
        b =
          (L.sum . makeFilterExpansionList rows cols rCenter cCenter $
           betaFunction freq gScale (L.last oris) (L.last scales)) /
          (2 * pi * gScale ^ (2 :: Int) :+ 0)
        a =
          (sqrt (L.last scales) :+ 0) /
          (sqrt . L.sum . makeFilterExpansionList rows cols rCenter cCenter $
           alphaFunction freq gScale (L.last oris) (L.last scales) b)
        filterTmp =
          VS.fromList . conjugateFunc filterType $
          makeFilterConvolutionList
            rows
            cols
            (morletWavelet freq gScale (L.last oris) (L.last scales) a b)
    lock <- getFFTWLock
    (p1, vec) <- dft2dPlan lock plan rows cols filterTmp
    (p2, _) <- idft2dPlan lock p1 rows cols vec
    filters <-
      Filter params <$!>
      M.mapM (dftExecuteBatch p2 (DFTPlanID DFT2D [rows, cols] [])) filterList
    return (p2, filters)
  {-# INLINE getFilterConvolutionNum #-}
  getFilterConvolutionNum (Filter (MorletWaveletParams _ _ _ _ oris scales) _) =
    L.length scales * L.length oris
  {-# INLINE applyFilterConvolution #-}
  applyFilterConvolution plan (Filter (MorletWaveletParams rows cols _ _ _ _) filters) xs = do
    ys <- dftExecuteBatch plan (DFTPlanID DFT2D [rows, cols] []) xs
    dftExecuteBatch plan (DFTPlanID IDFT2D [rows, cols] []) .
      L.concatMap (\x -> L.concatMap (L.map (VS.zipWith (*) x)) filters) $
      ys
  {-# INLINE getFilterConvolutionList #-}
  getFilterConvolutionList = L.concat . getFilter

{-# INLINE morletWavelet #-}

morletWavelet
  :: Double
  -> Double
  -> Double
  -> Double
  -> Complex Double
  -> Complex Double
  -> Int
  -> Int
  -> Complex Double
morletWavelet freq scale ori a alpha beta x y =
  alpha * (exp (0 :+ (freq * x' / a)) - beta) *
  (exp (-r / (2 * s2) / (a * a)) :+ 0) /
  (sqrt a :+ 0)
  where
    r = x' ^ (2 :: Int) + (1 * y') ^ (2 :: Int)
    x' = fromIntegral x * cos ori + fromIntegral y * sin ori
    y' = fromIntegral (-x) * sin ori + fromIntegral y * cos ori
    s2 = scale * scale
      
{-# INLINE betaFunction #-}

betaFunction :: Double -> Double -> Double -> Double -> Int -> Int -> Complex Double
betaFunction freq sigma theta a x y =
  (exp (-r / (2 * (a * sigma) ^ (2 :: Int))) :+ 0) * exp (0 :+ freq * x' / a)
  where
    r = fromIntegral $ x ^ (2 :: Int) + y ^ (2 :: Int)
    x' = fromIntegral x * cos theta + fromIntegral y * sin theta
    
{-# INLINE alphaFunction #-}

alphaFunction :: Double -> Double -> Double -> Double -> Complex Double -> Int -> Int -> Complex Double
alphaFunction freq sigma theta a beta x y =
  (exp (-r / ((a * sigma) ^ (2 :: Int))) :+ 0) *
  (1 + beta ^ (2 :: Int) - 2 * beta * (cos (freq * x' / a) :+ 0))
  where
    r = fromIntegral $ x ^ (2 :: Int) + y ^ (2 :: Int)
    x' = fromIntegral x * cos theta + fromIntegral y * sin theta
