{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}
module CV.Filter.FourierMellinTransform
  ( module F
  , FourierMellinTransformExpansion
  , FourierMellinTransformConvolution
  , fourierMellinTransform
  , makeFourierMellinTransformFilterExpansion
  , makeFourierMellinTransformFilterConvolution
  , applyFourierMellinTransformFilterConvolution
  ) where

import           Control.Arrow
import           Control.Monad                      as M
import           CV.Filter                          as F
import           CV.Filter.PolarSeparableFilterType as F
import           CV.Utility.Coordinates
import           Data.Complex                       as C
import           Data.List                          as L
import           Data.Vector.Storable               as VS
import           Data.Vector.Unboxed                as VU


newtype FourierMellinTransformExpansion =
  FourierMellinTransformExpansion (Filter PolarSeparableFilterParams [[VU.Vector (Complex Double)]])
newtype FourierMellinTransformConvolution =
  FourierMellinTransformConvolution (Filter PolarSeparableFilterParams [[VS.Vector (Complex Double)]])

instance FilterExpansion FourierMellinTransformExpansion where
  type FilterExpansionParameters FourierMellinTransformExpansion = PolarSeparableFilterParams
  {-# INLINE makeFilterExpansion #-}
  makeFilterExpansion params@(FourierMellinTransformParams rows cols rfs afs) rCenter cCenter =
    FourierMellinTransformExpansion . Filter params $
    makeFourierMellinTransformFilterExpansion params rCenter cCenter
  {-# INLINE getFilterExpansionNum #-}
  getFilterExpansionNum (FourierMellinTransformExpansion (Filter (FourierMellinTransformParams _ _ rfs afs) _)) =
    L.length rfs * L.length afs
  {-# INLINE applyFilterExpansion #-}
  applyFilterExpansion (FourierMellinTransformExpansion (Filter _ filters)) =
    L.concatMap (\x -> L.concatMap (L.map (VU.sum . VU.zipWith (*) x)) filters)
  {-# INLINE getFilterExpansionList #-}
  getFilterExpansionList (FourierMellinTransformExpansion x) =
    L.concat . getFilter $ x


instance FilterConvolution FourierMellinTransformConvolution where
  type FilterConvolutionParameters FourierMellinTransformConvolution = PolarSeparableFilterParams
  {-# INLINE makeFilterConvolution #-}
  makeFilterConvolution plan params filterType = do
    second (FourierMellinTransformConvolution . Filter params) <$>
      makeFourierMellinTransformFilterConvolution plan params filterType
    error
      "makeFilterConvolution: FourierMellinTransform: filter parameter type error."
  {-# INLINE getFilterConvolutionNum #-}
  getFilterConvolutionNum (FourierMellinTransformConvolution (Filter (FourierMellinTransformParams _ _ rfs afs) _)) =
    L.length rfs * L.length afs
  {-# INLINE applyFilterConvolution #-}
  applyFilterConvolution plan (FourierMellinTransformConvolution (Filter (FourierMellinTransformParams rows cols _ _) filters)) xs = do
    ys <- dftExecuteBatch plan (DFTPlanID DFT2D [rows, cols] []) xs
    dftExecuteBatch plan (DFTPlanID IDFT2D [rows, cols] []) .
      L.concatMap (\x -> L.concatMap (L.map (VS.zipWith (*) x)) filters) $
      ys
  {-# INLINE applyInvariantFilterConvolution #-}
  applyInvariantFilterConvolution plan (FourierMellinTransformConvolution (Filter (FourierMellinTransformParams rows cols _ _) filters)) xs =
    applyFourierMellinTransformFilterConvolution plan rows cols filters xs
  {-# INLINE getFilterConvolutionList #-}
  getFilterConvolutionList (FourierMellinTransformConvolution x) =
    L.concat . getFilter $ x

{-# INLINE fourierMellinTransform #-}

fourierMellinTransform ::  Double -> Int -> Int -> Int -> Complex Double
fourierMellinTransform rf af x y
  | x == 0 && y == 0 = 0
  | otherwise = (r ** ((-1) :+ (-rf))) * exp (0 :+ (fromIntegral (-af) * theta))
  where
    r = sqrt . fromIntegral $ x ^ (2 :: Int) + y ^ (2 :: Int)
    theta = angleFunctionRad (fromIntegral x) (fromIntegral y)

{-# INLINE makeFourierMellinTransformFilterExpansion #-}

makeFourierMellinTransformFilterExpansion :: PolarSeparableFilterParams
                                          -> Int
                                          -> Int
                                          -> [[VU.Vector (Complex Double)]]
makeFourierMellinTransformFilterExpansion (FourierMellinTransformParams rows cols rfs afs) rCenter cCenter =
  [ [ VU.fromList $
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
makeFourierMellinTransformFilterExpansion _ _ _ =
  error
    "makeFourierMellinTransformFilterExpansion : filter parameter type error."

{-# INLINE makeFourierMellinTransformFilterConvolution #-}

makeFourierMellinTransformFilterConvolution
  :: DFTPlan
  -> PolarSeparableFilterParams
  -> ConvolutionalFilterType
  -> IO (DFTPlan, [[VS.Vector (Complex Double)]])
makeFourierMellinTransformFilterConvolution plan (FourierMellinTransformParams rows cols rfs afs) filterType = do
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
    M.mapM (dftExecuteBatch p2 (DFTPlanID DFT2D [rows, cols] [])) filterList
  return (p2, filters)
makeFourierMellinTransformFilterConvolution _ _ _ =
  error
    "makeFourierMellinTransformFilterConvolution: filter parameter type error."
    
{-# INLINE applyFourierMellinTransformFilterConvolution #-}

applyFourierMellinTransformFilterConvolution
  :: DFTPlan
  -> Int
  -> Int
  -> [[VS.Vector (Complex Double)]]
  -> [VS.Vector (Complex Double)]
  -> IO [[VS.Vector (Complex Double)]]
applyFourierMellinTransformFilterConvolution plan rows cols filters xs = do
  ys <- dftExecuteBatch plan (DFTPlanID DFT2D [rows, cols] []) xs
  fmap (\x -> [x]) .
    dftExecuteBatch plan (DFTPlanID IDFT2D [rows, cols] []) .
    L.concatMap (\x -> L.concatMap (L.map (VS.zipWith (*) x)) filters) $
    ys
