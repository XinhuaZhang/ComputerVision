{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}
module CV.Filter.FourierMellinTransform
  ( module F
  , FourierMellinTransformParams(..)
  , FourierMellinTransformExpansion
  , FourierMellinTransformConvolution
  ) where

import           Control.Monad          as M
import           CV.Filter              as F
import           CV.Utility.Coordinates
import           Data.Complex           as C
import           Data.List              as L
import           Data.Vector.Storable   as VS
import           Data.Vector.Unboxed    as VU

data FourierMellinTransformParams = FourierMellinTransformParams
  { getFourierMellinTransformRows        :: !Int
  , getFourierMellinTransformCols        :: !Int
  , getFourierMellinTransformRadialFreq  :: ![Double]
  , getFourierMellinTransformAngularFreq :: ![Int]
  } deriving (Show, Read)

type FourierMellinTransformExpansion = Filter FourierMellinTransformParams [[VU.Vector (Complex Double)]]
type FourierMellinTransformConvolution = Filter FourierMellinTransformParams [[VS.Vector (Complex Double)]]

instance FilterExpansion FourierMellinTransformExpansion where
  type FilterExpansionParameters FourierMellinTransformExpansion = FourierMellinTransformParams
  {-# INLINE makeFilterExpansion #-}
  makeFilterExpansion params@(FourierMellinTransformParams rows cols rfs afs) rCenter cCenter =
    Filter params $!
    [ [ VU.fromListN (rows * cols) $
    makeFilterExpansionList
      rows
      cols
      rCenter
      cCenter
      (fourierMellinTransform rf af)
    | af <- afs
    ]
    | rf <- rfs
    ]
  {-# INLINE getFilterExpansionNum #-}
  getFilterExpansionNum (Filter (FourierMellinTransformParams _ _ rfs afs) _) =
    L.length rfs * L.length afs
  {-# INLINE applyFilterExpansion #-}
  applyFilterExpansion (Filter _ filters) =
    L.concatMap (\x -> L.concatMap (L.map (VU.sum . VU.zipWith (*) x)) filters)
  {-# INLINE getFilterExpansionList #-}
  getFilterExpansionList = L.concat . getFilter


instance FilterConvolution FourierMellinTransformConvolution where
  type FilterConvolutionParameters FourierMellinTransformConvolution = FourierMellinTransformParams
  {-# INLINE makeFilterConvolution #-}
  makeFilterConvolution plan params@(FourierMellinTransformParams rows cols rfs afs) filterType = do
    let filterTemp =
          VS.fromList . conjugateFunc filterType $!
          makeFilterConvolutionList
            rows
            cols
            (fourierMellinTransform (L.last rfs) (L.last afs))
        filterList =
          L.map
            (\rf ->
               L.map
                 (\af ->
                    VS.fromList . conjugateFunc filterType $!
                    makeFilterConvolutionList
                      rows
                      cols
                      (fourierMellinTransform rf af))
                 afs)
            rfs
    lock <- getFFTWLock
    (p1, vec) <- dft2dPlan lock plan rows cols filterTemp
    (p2, _) <- idft2dPlan lock p1 rows cols vec
    filters <-
      Filter params <$!>
      M.mapM (dftExecuteBatch p2 (DFTPlanID DFT2D [rows, cols] [])) filterList
    return (p2, filters)
  {-# INLINE getFilterConvolutionNum #-}
  getFilterConvolutionNum (Filter (FourierMellinTransformParams _ _ rfs afs) _) =
    L.length rfs * L.length afs
  {-# INLINE applyFilterConvolution #-}
  applyFilterConvolution plan (Filter (FourierMellinTransformParams rows cols _ _) filters) xs = do
    ys <- dftExecuteBatch plan (DFTPlanID DFT2D [rows, cols] []) xs
    dftExecuteBatch plan (DFTPlanID IDFT2D [rows, cols] []) .
      L.concatMap (\x -> L.concatMap (L.map (VS.zipWith (*) x)) filters) $
      ys
  {-# INLINE getFilterConvolutionList #-}
  getFilterConvolutionList = L.concat . getFilter

{-# INLINE fourierMellinTransform #-}

fourierMellinTransform ::  Double -> Int -> Int -> Int -> Complex Double
fourierMellinTransform rf af x y
  | x == 0 && y == 0 = 0
  | otherwise = (r ** ((-1) :+ (-rf))) * exp (0 :+ (fromIntegral (-af) * theta))
  where
    r = sqrt . fromIntegral $ x ^ (2 :: Int) + y ^ (2 :: Int)
    theta = angleFunctionRad (fromIntegral x) (fromIntegral y)
