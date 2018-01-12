{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}

module CV.Filter.PinwheelBlob
  ( module F
  , PinwheelBlobParams(..)
  , PinwheelBlobExpansion
  , PinwheelBlobConvolution
  , applyPinwheelBlobConvolution
  ) where

import           Control.Monad            as M
import           CV.Array.Image
import           CV.Filter                as F
import           CV.Filter.GaussianFilter
import           CV.Filter.MorletWavelet
import           CV.Image
import           CV.Utility.Coordinates
import           CV.Utility.DFT
import           CV.Utility.Parallel
import           Data.Array.Repa          as R
import           Data.Complex             as C
import           Data.List                as L
import           Data.Vector.Storable     as VS
import           Data.Vector.Unboxed      as VU

data PinwheelBlobParams = PinwheelBlobParams
  { pinwheelBlobRows          :: !Int
  , pinwheelBlobCols          :: !Int
  , pinwheelBlobGaussianScale :: !Double
  , pinwheelBlobScale         :: ![Double]
  , pinwheelBlobFreqs         :: !Double
  , pinwheelBlobOrientation   :: ![Double]
  , pinwheelBlobThetaShift    :: ![Double]
  , pinwheelBlobRadiusShift   :: ![Double]
  } deriving (Show, Read)

type PinwheelBlobExpansion = Filter PinwheelBlobParams [[VU.Vector (Complex Double)]]
type PinwheelBlobConvolution = Filter PinwheelBlobParams [[VS.Vector (Complex Double)]]

instance FilterExpansion PinwheelBlobExpansion where
  type FilterExpansionParameters PinwheelBlobExpansion = PinwheelBlobParams
  {-# INLINE makeFilterExpansion #-}
  makeFilterExpansion params@(PinwheelBlobParams rows cols gScale wScales freq oris tShifts rShifts) rCenter cCenter =
    let morletWaveletFilterParams =
          MorletWaveletParams
          { morletWaveletRows = 128 -- for theta
          , morletWaveletCols = div (min rows cols) 2 -- for ridus
          , morletWaveletFreq = freq
          , morletWaveletGaussianScale = gScale
          , morletWaveletOrientation = oris
          , morletWaveletScale = wScales
          }
        logPolarFilterList =
          [ ( (tShift, rShift)
            , getFilterExpansionList
                (makeFilterExpansion
                   morletWaveletFilterParams
                   (round tShift)
                   (round rShift) :: MorletWaveletExpansion))
          | tShift <- tShifts
          , rShift <- rShifts
          ]
        cartesianFilterListFunc f =
          L.map
            (\((tShift, rShift), xs) ->
               let polarR =
                     fromIntegral . morletWaveletCols $
                     morletWaveletFilterParams
                   deltaTheta =
                     2 * pi /
                     (fromIntegral . morletWaveletRows $
                      morletWaveletFilterParams)
                   deltaR =
                     (log polarR) /
                     (fromIntegral . morletWaveletCols $
                      morletWaveletFilterParams)
                   theta = tShift * deltaTheta
                   r = exp (rShift * deltaR)
               in L.map
                    (getCartesianImage .
                     logpolar2CartesianImage
                       rows
                       cols
                       ( (fromIntegral rCenter) -- - r * cos theta
                       , (fromIntegral cCenter) -- - r * sin theta
                        ) .
                     LogpolarImage polarR (-10000, 10000) .
                     R.fromUnboxed
                       (Z :. (1 :: Int) :.
                        (morletWaveletRows morletWaveletFilterParams) :.
                        (morletWaveletCols morletWaveletFilterParams)) .
                     VU.map f)
                    xs)
            logPolarFilterList
        cartesianFilterList =
          L.zipWith
            (L.zipWith
               (\arr1 arr2 ->
                  toUnboxed . computeS $
                  traverse2
                    arr1
                    arr2
                    (\sh1 _ -> sh1)
                    (\f1 f2 sh -> f1 sh :+ f2 sh)))
            (cartesianFilterListFunc realPart)
            (cartesianFilterListFunc imagPart)
    in Filter params cartesianFilterList
  {-# INLINE getFilterExpansionNum #-}
  getFilterExpansionNum (Filter (PinwheelBlobParams _ _ _ wScales _ oris tShifts rShifts) _) =
    L.length wScales * L.length oris * L.length tShifts * L.length rShifts
  {-# INLINE getFilterExpansionList #-}
  getFilterExpansionList = L.concat . getFilter
  {-# INLINE applyFilterExpansion #-}
  applyFilterExpansion _ = error "applyFilterExpansion: not implemented yet."

instance FilterConvolution PinwheelBlobConvolution where
  type FilterConvolutionParameters PinwheelBlobConvolution = PinwheelBlobParams
  {-# INLINE makeFilterConvolution #-}
  makeFilterConvolution plan params@(PinwheelBlobParams rows cols gScale wScales freq oris tShifts rShifts) filterType = do
    let (Filter _ filterList) =
          (makeFilterExpansion params (div rows 2) (div cols 2) :: PinwheelBlobExpansion)
        (Filter _ filterList1) =
          (makeFilterExpansion params (div rows 2) (div cols 2) :: PinwheelBlobExpansion)
        filterTmp =
          VS.fromList .
          conjugateFunc filterType .
          filterExpansionList2ConvolutionList rows cols . L.last . L.last $
          filterList1
    lock <- getFFTWLock
    (p1, vec) <- dft2dPlan lock plan rows cols filterTmp
    (p2, _) <- idft2dPlan lock p1 rows cols vec
    filters <-
      Filter params <$!>
      (M.mapM
         (dftExecuteBatch p2 (DFTPlanID DFT2D [rows, cols] []) .
          L.map VU.convert))
        filterList
    return (p2, filters)
  {-# INLINE getFilterConvolutionNum #-}
  getFilterConvolutionNum (Filter (PinwheelBlobParams _ _ _ wScales _ oris tShifts rShifts) _) =
    L.length wScales * L.length oris * L.length tShifts * L.length rShifts
  {-# INLINE applyFilterConvolution #-}
  applyFilterConvolution plan (Filter (PinwheelBlobParams rows cols _ _ _ _ _ _) filters) xs = do
    ys <- dftExecuteBatch plan (DFTPlanID DFT2D [rows, cols] []) xs
    dftExecuteBatch plan (DFTPlanID IDFT2D [rows, cols] []) .
      L.concatMap (\x -> L.concatMap (L.map (VS.zipWith (*) x)) filters) $
      ys
  {-# INLINE getFilterConvolutionList #-}
  getFilterConvolutionList = L.concat . getFilter

{-# INLINE filterExpansionList2ConvolutionList #-}

filterExpansionList2ConvolutionList :: Int
                                    -> Int
                                    -> VU.Vector (Complex Double)
                                    -> [Complex Double]
filterExpansionList2ConvolutionList rows cols vec =
  let arr = fromUnboxed (Z :. rows :. cols) vec
  in [ let rCenter = div rows 2
           cCenter = div cols 2
           x =
             if r < (rows `div` 2)
               then r
               else r - rows
           y =
             if c < (cols `div` 2)
               then c
               else c - cols
       in arr R.! (Z :. (x + rCenter) :. (y + cCenter))
     | r <- [0 .. rows - 1]
     , c <- [0 .. cols - 1]
     ]

applyPinwheelBlobConvolution
  :: DFTPlan
  -> PinwheelBlobConvolution
  -> [VS.Vector (Complex Double)]
  -> IO [[VS.Vector (Complex Double)]]
applyPinwheelBlobConvolution plan (Filter (PinwheelBlobParams rows cols _ _ _ _ _ _) filters) xs = do
  ys <- dftExecuteBatch plan (DFTPlanID DFT2D [rows, cols] []) xs
  M.mapM
    (dftExecuteBatch plan (DFTPlanID IDFT2D [rows, cols] []) .
     L.concatMap (\filter -> L.map (VS.zipWith (*) filter) ys))
    filters
