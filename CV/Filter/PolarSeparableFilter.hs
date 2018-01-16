module CV.Filter.PolarSeparableFilter
  ( module CV.Filter.PolarSeparableFilter
  , module CV.Filter.FourierMellinTransform
  , module CV.Filter.GaussianPinwheel
  , module CV.Filter.PinwheelFan
  , module CV.Filter.PinwheelRing
  , module CV.Filter.PinwheelBlob
  ) where

import           Control.Arrow
import           CV.Filter.FourierMellinTransform
import           CV.Filter.GaussianPinwheel
import           CV.Filter.PinwheelBlob
import           CV.Filter.PinwheelFan
import           CV.Filter.PinwheelRing
import           Data.Complex
import           Data.Vector.Storable             as VS

{-# INLINE makePolarSeparableFilterExpansion #-}

makePolarSeparableFilterExpansion :: PolarSeparableFilterParams
                                  -> PolarSeparableFilter PolarSeparableFilterExpansion
makePolarSeparableFilterExpansion params@(FourierMellinTransformParams rows cols _ _) =
  PolarSeparableFilter params . FourierMellinFilterExpansion $
  makeFourierMellinTransformFilterExpansion params (div rows 2) (div cols 2)
makePolarSeparableFilterExpansion params@(GaussianPinwheelParams rows cols _ _ _) =
  PolarSeparableFilter params . GaussianPinwheelFilterExpansion $
  makeGaussianPinwheelFilterExpansion params (div rows 2) (div cols 2)
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
makePolarSeparableFilterConvolution plan params@(FourierMellinTransformParams _ _ _ _) = do
  second (PolarSeparableFilter params . FourierMellinFilterConvolution) <$>
    (makeFourierMellinTransformFilterConvolution getEmptyPlan params Normal)
makePolarSeparableFilterConvolution plan params@(GaussianPinwheelParams _ _ _ _ _) = do
  second (PolarSeparableFilter params . GaussianPinwheelFilterConvolution) <$>
    (makeGaussianPinwheelFilterConvolution getEmptyPlan params Normal)
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
applyPolarSeparableInvariantFilterConvolution plan (PolarSeparableFilter (FourierMellinTransformParams rows cols _ _) (FourierMellinFilterConvolution filter)) =
  applyFourierMellinTransformFilterConvolution plan rows cols filter
applyPolarSeparableInvariantFilterConvolution plan (PolarSeparableFilter (GaussianPinwheelParams rows cols _ _ _) (GaussianPinwheelFilterConvolution filter)) =
  applyGaussianPinwheelFilterConvolution plan rows cols filter
applyPolarSeparableInvariantFilterConvolution plan (PolarSeparableFilter (PinwheelFanParams rows cols _ _ _ _ _) (PinwheelFanFilterConvolution filter)) =
  applyPinwheelFanFilterConvolution plan rows cols filter
applyPolarSeparableInvariantFilterConvolution plan (PolarSeparableFilter (PinwheelRingParams rows cols _ _ _ _ _) (PinwheelRingFilterConvolution filter)) =
  applyPinwheelRingFilterConvolution plan rows cols filter  
applyPolarSeparableInvariantFilterConvolution plan (PolarSeparableFilter (PinwheelBlobParams rows cols _ _ _ _ _ _) (PinwheelBlobFilterConvolution filter)) =
  applyPinwheelBlobFilterConvolution plan rows cols filter
