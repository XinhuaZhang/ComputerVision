module CV.Filter.PinwheelWavelet
  ( module CV.Filter.PinwheelWavelet
  , module CV.Filter.PinwheelBlob
  , module CV.Filter.PinwheelFan
  , module CV.Filter.PinwheelRing
  ) where

import           Control.Arrow
import           CV.Filter.PinwheelBlob
import           CV.Filter.PinwheelFan
import           CV.Filter.PinwheelRing
import           Data.Complex           as C
import           Data.List              as L
import           Data.Vector.Storable   as VS
import           Data.Vector.Unboxed    as VU

data PinwheelWaveletParams
  = PinwheelWaveletFanParams PinwheelFanParams
  | PinwheelWaveletRingParams PinwheelRingParams
  | PinwheelWaveletBlobParams PinwheelBlobParams
  deriving (Show, Read)

data PinwheelWaveletFilterExpansion
  = PinwheelWaveletFilterExpansionFan PinwheelFanExpansion
  | PinwheelWaveletFilterExpansionBlob PinwheelBlobExpansion
  | PinwheelWaveletFilterExpansionRing PinwheelRingExpansion

data PinwheelWaveletFilterConvolution
  = PinwheelWaveletFilterConvolutionFan PinwheelFanConvolution
  | PinwheelWaveletFilterConvolutionBlob PinwheelBlobConvolution
  | PinwheelWaveletFilterConvolutionRing PinwheelRingConvolution


makePinwheelWaveletFilterExpansion :: PinwheelWaveletParams
                                   -> PinwheelWaveletFilterExpansion
makePinwheelWaveletFilterExpansion (PinwheelWaveletFanParams params@(PinwheelFanParams rows cols _ _ _ _ _)) =
  PinwheelWaveletFilterExpansionFan 
  (makeFilterExpansion params (div rows 2) (div cols 2) :: PinwheelFanExpansion)
makePinwheelWaveletFilterExpansion (PinwheelWaveletRingParams params@(PinwheelRingParams rows cols _ _ _ _ _)) =
  PinwheelWaveletFilterExpansionRing 
  (makeFilterExpansion params (div rows 2) (div cols 2) :: PinwheelRingExpansion)
makePinwheelWaveletFilterExpansion (PinwheelWaveletBlobParams params@(PinwheelBlobParams rows cols _ _ _ _ _ _)) =
  PinwheelWaveletFilterExpansionBlob
    (makeFilterExpansion params (div rows 2) (div cols 2) :: PinwheelBlobExpansion)

makePinwheelWaveletFilterConvolution
  :: DFTPlan
  -> PinwheelWaveletParams
  -> ConvolutionalFilterType
  -> IO (DFTPlan, PinwheelWaveletFilterConvolution)
makePinwheelWaveletFilterConvolution plan (PinwheelWaveletFanParams params@(PinwheelFanParams rows cols _ _ _ _ _)) filterType =
  second PinwheelWaveletFilterConvolutionFan <$>
  (makeFilterConvolution plan params filterType :: IO (DFTPlan, PinwheelFanConvolution))
makePinwheelWaveletFilterConvolution plan (PinwheelWaveletRingParams params@(PinwheelRingParams rows cols _ _ _ _ _)) filterType =
  second PinwheelWaveletFilterConvolutionRing <$>
  (makeFilterConvolution plan params filterType :: IO (DFTPlan, PinwheelRingConvolution))
makePinwheelWaveletFilterConvolution plan (PinwheelWaveletBlobParams params@(PinwheelBlobParams rows cols _ _ _ _ _ _)) filterType =
  second PinwheelWaveletFilterConvolutionBlob <$>
  (makeFilterConvolution plan params filterType :: IO (DFTPlan, PinwheelBlobConvolution))

applyPinwheelWaveletFilterConvolution
  :: DFTPlan
  -> PinwheelWaveletFilterConvolution
  -> [VS.Vector (Complex Double)]
  -> IO [[VS.Vector (Complex Double)]]
applyPinwheelWaveletFilterConvolution plan (PinwheelWaveletFilterConvolutionFan filter) =
  applyPinwheelFanConvolution plan filter
applyPinwheelWaveletFilterConvolution plan (PinwheelWaveletFilterConvolutionRing filter) =
  applyPinwheelRingConvolution plan filter  
applyPinwheelWaveletFilterConvolution plan (PinwheelWaveletFilterConvolutionBlob filter) =
  applyPinwheelBlobConvolution plan filter
