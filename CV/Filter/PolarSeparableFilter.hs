module CV.Filter.PolarSeparableFilter
  ( module CV.Filter.PolarSeparableFilter
  , module CV.Filter.FourierMellinTransform
  , module CV.Filter.GaussianPinwheel
  , module CV.Filter.InverseGaussianPinwheel
  , module CV.Filter.PinwheelFan
  , module CV.Filter.PinwheelRing
  , module CV.Filter.PinwheelBlob
  ) where

import           Control.Arrow
import           CV.Filter.FourierMellinTransform
import           CV.Filter.GaussianPinwheel
import           CV.Filter.InverseGaussianPinwheel
import           CV.Filter.PinwheelBlob
import           CV.Filter.PinwheelFan
import           CV.Filter.PinwheelRing
import           Data.Complex
import           Data.List                         as L
import           Data.Vector.Storable              as VS

{-# INLINE makePolarSeparableFilterExpansion #-}

makePolarSeparableFilterExpansion :: PolarSeparableFilterParams
                                  -> PolarSeparableFilter PolarSeparableFilterExpansion
makePolarSeparableFilterExpansion params@(FourierMellinTransformParams rows cols _ _ _) =
  PolarSeparableFilter params . FourierMellinFilterExpansion $
  makeFourierMellinTransformFilterExpansion params (div rows 2) (div cols 2)
makePolarSeparableFilterExpansion params@(GaussianPinwheelParams rows cols _ _ _) =
  PolarSeparableFilter params . GaussianPinwheelFilterExpansion $
  makeGaussianPinwheelFilterExpansion params (div rows 2) (div cols 2)
makePolarSeparableFilterExpansion params@(InverseGaussianPinwheelParams rows cols  _ _ _) =
  PolarSeparableFilter params . InverseGaussianPinwheelFilterExpansion $
  makeInverseGaussianPinwheelFilterExpansion params (div rows 2) (div cols 2)
makePolarSeparableFilterExpansion params@(PinwheelFanParams rows cols _ _ _ _ _) =
  PolarSeparableFilter params . PinwheelFanFilterExpansion $
  makePinwheelFanFilterExpansion params (div rows 2) (div cols 2)
makePolarSeparableFilterExpansion params@(PinwheelRingParams rows cols _ _ _ _ _) =
  PolarSeparableFilter params . PinwheelRingFilterExpansion $
  makePinwheelRingFilterExpansion params (div rows 2) (div cols 2)
makePolarSeparableFilterExpansion params@(PinwheelBlobParams rows cols _ _ _ _ _ _) =
  PolarSeparableFilter params . PinwheelBlobFilterExpansion $
  makePinwheelBlobFilterExpansion params (div rows 2) (div cols 2)

{-# INLINE makePolarSeparableFilterConvolution #-}

makePolarSeparableFilterConvolution
  :: DFTPlan
  -> PolarSeparableFilterParams
  -> IO (DFTPlan, PolarSeparableFilter PolarSeparableFilterConvolution)
makePolarSeparableFilterConvolution plan params@(FourierMellinTransformParams _ _ _ _ _) = do
  second (PolarSeparableFilter params . FourierMellinFilterConvolution) <$>
    (makeFourierMellinTransformFilterConvolution getEmptyPlan params Normal)
makePolarSeparableFilterConvolution plan params@(GaussianPinwheelParams _ _ _ _ _) = do
  second (PolarSeparableFilter params . GaussianPinwheelFilterConvolution) <$>
    (makeGaussianPinwheelFilterConvolution getEmptyPlan params Normal)
makePolarSeparableFilterConvolution plan params@(InverseGaussianPinwheelParams  _ _ _ _ _) = do
  second (PolarSeparableFilter params . InverseGaussianPinwheelFilterConvolution) <$>
    (makeInverseGaussianPinwheelFilterConvolution getEmptyPlan params Normal)
makePolarSeparableFilterConvolution plan params@(PinwheelFanParams _ _ _ _ _ _ _) = do
  second (PolarSeparableFilter params . PinwheelFanFilterConvolution) <$>
    (makePinwheelFanFilterConvolution getEmptyPlan params Normal)
makePolarSeparableFilterConvolution plan params@(PinwheelRingParams _ _ _ _ _ _ _) = do
  second (PolarSeparableFilter params . PinwheelRingFilterConvolution) <$>
    (makePinwheelRingFilterConvolution getEmptyPlan params Normal)
makePolarSeparableFilterConvolution plan params@(PinwheelBlobParams _ _ _ _ _ _ _ _) = do
  second (PolarSeparableFilter params . PinwheelBlobFilterConvolution) <$>
    (makePinwheelBlobFilterConvolution getEmptyPlan params Normal)

{-# INLINE makePolarSeparableFilterConvolutionList #-}

makePolarSeparableFilterConvolutionList
  :: DFTPlan
  -> [PolarSeparableFilterParams]
  -> IO (DFTPlan, [PolarSeparableFilter PolarSeparableFilterConvolution])
makePolarSeparableFilterConvolutionList plan [] = return (plan, [])
makePolarSeparableFilterConvolutionList plan (x:xs) = do
  (p1, y) <- makePolarSeparableFilterConvolution plan x
  second ((:) y) <$> makePolarSeparableFilterConvolutionList p1 xs

{-# INLINE applyPolarSeparableInvariantFilterConvolution #-}

applyPolarSeparableInvariantFilterConvolution
  :: DFTPlan
  -> PolarSeparableFilter PolarSeparableFilterConvolution
  -> [VS.Vector (Complex Double)]
  -> IO [[VS.Vector (Complex Double)]]
applyPolarSeparableInvariantFilterConvolution plan (PolarSeparableFilter (FourierMellinTransformParams rows cols _ _ _) (FourierMellinFilterConvolution filter)) =
  applyFourierMellinTransformFilterConvolution plan rows cols filter
applyPolarSeparableInvariantFilterConvolution plan (PolarSeparableFilter (GaussianPinwheelParams rows cols _ _ _) (GaussianPinwheelFilterConvolution filter)) =
  applyGaussianPinwheelFilterConvolution plan rows cols filter
applyPolarSeparableInvariantFilterConvolution plan (PolarSeparableFilter (InverseGaussianPinwheelParams rows cols  _ _ _) (InverseGaussianPinwheelFilterConvolution filter)) =
  applyInverseGaussianPinwheelFilterConvolution plan rows cols filter
applyPolarSeparableInvariantFilterConvolution plan (PolarSeparableFilter (PinwheelFanParams rows cols _ _ _ _ _) (PinwheelFanFilterConvolution filter)) =
  applyPinwheelFanFilterConvolution plan rows cols filter
applyPolarSeparableInvariantFilterConvolution plan (PolarSeparableFilter (PinwheelRingParams rows cols _ _ _ _ _) (PinwheelRingFilterConvolution filter)) =
  applyPinwheelRingFilterConvolution plan rows cols filter
applyPolarSeparableInvariantFilterConvolution plan (PolarSeparableFilter (PinwheelBlobParams rows cols _ _ _ _ _ _) (PinwheelBlobFilterConvolution filter)) =
  applyPinwheelBlobFilterConvolution plan rows cols filter


{-# INLINE makePolarSeparableFilterConvolutionPI #-}

makePolarSeparableFilterConvolutionPI
  :: DFTPlan
  -> PolarSeparableFilterParams
  -> IO (DFTPlan, PolarSeparableFilter PolarSeparableFilterConvolution)
makePolarSeparableFilterConvolutionPI plan params@(FourierMellinTransformParams _ _ _ _ _) = do
  second (PolarSeparableFilter params . FourierMellinFilterConvolution) <$>
    (makeFourierMellinTransformFilterConvolutionPI getEmptyPlan params Normal)
makePolarSeparableFilterConvolutionPI plan params@(GaussianPinwheelParams _ _ _ _ _) = do
  second (PolarSeparableFilter params . GaussianPinwheelFilterConvolution) <$>
    (makeGaussianPinwheelFilterConvolutionPI getEmptyPlan params Normal)
makePolarSeparableFilterConvolutionPI plan params@(InverseGaussianPinwheelParams _ _ _ _ _) = do
  second (PolarSeparableFilter params . InverseGaussianPinwheelFilterConvolution) <$>
    (makeInverseGaussianPinwheelFilterConvolutionPI getEmptyPlan params Normal)
-- makePolarSeparableFilterConvolutionPI plan params@(PinwheelFanParams _ _ _ _ _ _ _) = do
--   second (PolarSeparableFilter params . PinwheelFanFilterConvolution) <$>
--     (makePinwheelFanFilterConvolution getEmptyPlan params Normal)
makePolarSeparableFilterConvolutionPI plan params@(PinwheelRingParams _ _ _ _ _ _ _) = do
  second (PolarSeparableFilter params . PinwheelRingFilterConvolution) <$>
    (makePinwheelRingFilterConvolutionPI getEmptyPlan params Normal)
-- makePolarSeparableFilterConvolutionPI plan params@(PinwheelBlobParams _ _ _ _ _ _ _ _) = do
--   second (PolarSeparableFilter params . PinwheelBlobFilterConvolution) <$>
--     (makePinwheelBlobFilterConvolution getEmptyPlan params Normal)

{-# INLINE makePolarSeparableFilterConvolutionPIList #-}

makePolarSeparableFilterConvolutionPIList
  :: DFTPlan
  -> [PolarSeparableFilterParams]
  -> IO (DFTPlan, [PolarSeparableFilter PolarSeparableFilterConvolution])
makePolarSeparableFilterConvolutionPIList plan [] = return (plan, [])
makePolarSeparableFilterConvolutionPIList plan (x:xs) = do
  (p1, y) <- makePolarSeparableFilterConvolutionPI plan x
  second ((:) y) <$> makePolarSeparableFilterConvolutionPIList p1 xs


{-# INLINE applyPolarSeparableInvariantFilterConvolutionVariedSize #-}

applyPolarSeparableInvariantFilterConvolutionVariedSize
  :: DFTPlan
  -> PolarSeparableFilter PolarSeparableFilterConvolution
  -> Int
  -> Int
  -> [VS.Vector (Complex Double)]
  -> IO [[VS.Vector (Complex Double)]]
applyPolarSeparableInvariantFilterConvolutionVariedSize plan (PolarSeparableFilter (FourierMellinTransformParams _ _ rf af alpha) _) rows cols xs = do
  let params = FourierMellinTransformParams rows cols rf af alpha
  filter <- makeFourierMellinTransformFilterConvolutionFilter plan params Normal
  applyFourierMellinTransformFilterConvolution plan rows cols filter xs
applyPolarSeparableInvariantFilterConvolutionVariedSize _ (PolarSeparableFilter params _) _ _ _ =
  error $
  "applyPolarSeparableInvariantFilterConvolutionVariedSize: filter type is not supported.\n" L.++
  show params

{-# INLINE getPolarSeparableConvolutionFilterDims #-}

getPolarSeparableConvolutionFilterDims :: PolarSeparableFilter PolarSeparableFilterConvolution -> (Int,Int)
getPolarSeparableConvolutionFilterDims (PolarSeparableFilter (FourierMellinTransformParams rows cols _ _ _) _) =
  (rows, cols)
getPolarSeparableConvolutionFilterDims (PolarSeparableFilter params _) =
  error $
  "getPolarSeparableConvolutionFilterDims: filter type is not supported.\n" L.++
  show params


{-# INLINE makePolarSeparableFilterConvolutionPlan #-}

makePolarSeparableFilterConvolutionPlan :: DFTPlan
                                        -> PolarSeparableFilterParams
                                        -> [(Int, Int)]
                                        -> IO DFTPlan
makePolarSeparableFilterConvolutionPlan plan params@(FourierMellinTransformParams _ _ _ _ _) dims = do
  makeFourierMellinTransformFilterConvolutionPlan plan params Normal dims
makePolarSeparableFilterConvolutionPlan _ params _ =
  error $
  "makePolarSeparableFilterConvolutionPlan: filter type is not supported.\n" L.++
  show params
  

{-# INLINE makePolarSeparableFilterConvolutionFilter #-}

makePolarSeparableFilterConvolutionFilter :: DFTPlan
                                        -> PolarSeparableFilterParams
                                        -> IO [[VS.Vector (Complex Double)]]
makePolarSeparableFilterConvolutionFilter plan params@(FourierMellinTransformParams _ _ _ _ _) = do
  makeFourierMellinTransformFilterConvolutionFilter plan params Normal 
makePolarSeparableFilterConvolutionFilter _ params =
  error $
  "makePolarSeparableFilterConvolutionFilter: filter type is not supported.\n" L.++
  show params
