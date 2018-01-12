module CV.Filter.PolarSeparableFilter
  ( module CV.Filter.PolarSeparableFilter
  , module CV.Filter.FourierMellinTransform
  , module CV.Filter.GaussianPinwheel
  ) where

import           Control.Arrow
import           CV.Filter.FourierMellinTransform
import           CV.Filter.GaussianPinwheel
import           Data.Complex
import           Data.Vector.Storable             as VS

data PolarSeparableFilterParams
  = PolarSeparableFilterFourierMellinParams FourierMellinTransformParams
  | PolarSeparableFilterGaussianPinwheelParams GaussianPinwheelParams
  deriving (Show, Read)

data PolarSeparableFilterConvolution
  = PolarSeparableFilterConvolutionFourierMellin FourierMellinTransformConvolution
  | PolarSeparableFilterConvolutionGaussianPinwheel GaussianPinwheelConvolution
  
data PolarSeparableFilterExpansion
  = PolarSeparableFilterExpansionFourierMellin FourierMellinTransformExpansion
  | PolarSeparableFilterExpansionGaussianPinwheel GaussianPinwheelExpansion

makePolarSeparableFilterExpansion :: PolarSeparableFilterParams -> PolarSeparableFilterExpansion
makePolarSeparableFilterExpansion (PolarSeparableFilterFourierMellinParams params@(FourierMellinTransformParams rows cols _ _)) =
  PolarSeparableFilterExpansionFourierMellin $
  makeFilterExpansion params (div rows 2) (div cols 2)
makePolarSeparableFilterExpansion (PolarSeparableFilterGaussianPinwheelParams params@(GaussianPinwheelParams rows cols _ _ _)) =
  PolarSeparableFilterExpansionGaussianPinwheel $
  makeFilterExpansion params (div rows 2) (div cols 2)


makePolarSeparableFilterConvolution
  :: DFTPlan
  -> PolarSeparableFilterParams
  -> IO (DFTPlan, PolarSeparableFilterConvolution)
makePolarSeparableFilterConvolution plan (PolarSeparableFilterFourierMellinParams filterParams) = do
  second PolarSeparableFilterConvolutionFourierMellin <$>
    (makeFilterConvolution getEmptyPlan filterParams Normal :: IO (DFTPlan, FourierMellinTransformConvolution))
makePolarSeparableFilterConvolution plan (PolarSeparableFilterGaussianPinwheelParams filterParams) = do
  second PolarSeparableFilterConvolutionGaussianPinwheel <$>
    (makeFilterConvolution getEmptyPlan filterParams Normal :: IO (DFTPlan, GaussianPinwheelConvolution))

applyPolarSeparableFilterConvolution
  :: DFTPlan
  -> PolarSeparableFilterConvolution
  -> [VS.Vector (Complex Double)]
  -> IO [VS.Vector (Complex Double)]
applyPolarSeparableFilterConvolution plan (PolarSeparableFilterConvolutionFourierMellin filter) =
  applyFilterConvolution plan filter
applyPolarSeparableFilterConvolution plan (PolarSeparableFilterConvolutionGaussianPinwheel filter) =
  applyFilterConvolution plan filter
