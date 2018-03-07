module Application.ECCV2018.Utility where

import           CV.Filter.PolarSeparableFilter
import           Data.Complex
import           Data.List                      as L
import           Data.Vector.Storable           as VS

{-# INLINE filterParamsFunc #-}

filterParamsFunc :: Int
                 -> Int
                 -> Double
                 -> PolarSeparableFilterType
                 -> PolarSeparableFilterParams
filterParamsFunc rows cols alpha FourierMellinFilterType =
  FourierMellinTransformParams
  { getFourierMellinTransformRows = rows
  , getFourierMellinTransformCols = cols
  , getFourierMellinTransformRadialFreq = [0 .. 2]
  , getFourierMellinTransformAngularFreq = [-5 .. 5]
  , getFourierMellinTransformAlpha = alpha
  }
filterParamsFunc rows cols _ GaussianPinwheelFilterType =
  GaussianPinwheelParams
  { getGaussianPinwheelRows = rows
  , getGaussianPinwheelCols = cols
  , getGaussianPinwheelScale = L.map (* pi) [0.35]
  , getGaussianPinwheelRadialFreq = [0 .. 3]
  , getGaussianPinwheelAngularFreq = [-3 .. 3]
  }
filterParamsFunc rows cols _ PinwheelFanFilterType =
  PinwheelFanParams
  { pinwheelFanRows = rows
  , pinwheelFanCols = cols
  , pinwheelFanGaussianScale = 0.1 * pi
  , pinwheelFanScale = L.map (\x -> 2 ** (x / 1)) [0 .. 0]
  , pinwheelFanRadialFreqs = [0 .. 7] -- L.map (\x -> x / 8 * pi) [0, 6, 8, 10]
  , pinwheelFanAngularFreqs = [0 .. 7]
  , pinwheelFanTheta = L.map (* (2 * pi)) [0.05,0.1 .. 1]
  }
filterParamsFunc rows cols _ PinwheelRingFilterType =
  PinwheelRingParams
  { pinwheelRingRows = rows
  , pinwheelRingCols = cols
  , pinwheelRingGaussianScale = 0.1 * pi
  , pinwheelRingScale = L.map (\x -> 2 ** (x / 1)) [0 .. 0]
  , pinwheelRingRadialFreqs = [0 .. 3] -- L.map (\x -> x / 8 * pi) [0, 6, 8, 10]
  , pinwheelRingAngularFreqs = [-3 .. 3] --[-7, -6 ..- 4] L.++ [4 .. 7]
  , pinwheelRingRadius = [4..9]
  }
filterParamsFunc rows cols _ PinwheelBlobFilterType =
  PinwheelBlobParams
  { pinwheelBlobRows = rows
  , pinwheelBlobCols = cols
  , pinwheelBlobGaussianScale = 2 * pi
  , pinwheelBlobScale = [1]
  , pinwheelBlobFreqs = 0.5 * pi
  , pinwheelBlobOrientation = [0,10 .. 360 - 10]
  , pinwheelBlobThetaShift = [0,32 .. 127 - 32] -- [0,127]
  , pinwheelBlobRadiusShift = [22, 24, 26, 28] -- [0,32]
  }
filterParamsFunc rows cols _ InverseGaussianPinwheelFilterType =
  InverseGaussianPinwheelParams
  { getInverseGaussianPinwheelRows = rows
  , getInverseGaussianPinwheelCols = cols
  , getInverseGaussianPinwheelScale = [8]
  , getInverseGaussianPinwheelRadialFreq = [0 .. 9]
  , getInverseGaussianPinwheelAngularFreq = [-9 .. 9]
  }    
  

{-# INLINE invariantScatteringFilterParamsFunc #-}

invariantScatteringFilterParamsFunc :: Int
                 -> Int
                 -> PolarSeparableFilterType
                 -> PolarSeparableFilterParams
invariantScatteringFilterParamsFunc rows cols FourierMellinFilterType =
  FourierMellinTransformParams
  { getFourierMellinTransformRows = rows
  , getFourierMellinTransformCols = cols
  , getFourierMellinTransformRadialFreq = [0 .. 0]
  , getFourierMellinTransformAngularFreq = [-2 .. 2]
  , getFourierMellinTransformAlpha = 0
  }
invariantScatteringFilterParamsFunc rows cols GaussianPinwheelFilterType =
  GaussianPinwheelParams
  { getGaussianPinwheelRows = rows
  , getGaussianPinwheelCols = cols
  , getGaussianPinwheelScale = L.map (* pi) [0.35]
  , getGaussianPinwheelRadialFreq = [0 .. 3]
  , getGaussianPinwheelAngularFreq = [-3 .. 3]
  }
invariantScatteringFilterParamsFunc rows cols PinwheelFanFilterType =
  PinwheelFanParams
  { pinwheelFanRows = rows
  , pinwheelFanCols = cols
  , pinwheelFanGaussianScale = 0.1 * pi
  , pinwheelFanScale = L.map (\x -> 2 ** (x / 1)) [0 .. 0]
  , pinwheelFanRadialFreqs = [0 .. 7] -- L.map (\x -> x / 8 * pi) [0, 6, 8, 10]
  , pinwheelFanAngularFreqs = [0 .. 7]
  , pinwheelFanTheta = L.map (* (2 * pi)) [0.05,0.1 .. 1]
  }
invariantScatteringFilterParamsFunc rows cols PinwheelRingFilterType =
  PinwheelRingParams
  { pinwheelRingRows = rows
  , pinwheelRingCols = cols
  , pinwheelRingGaussianScale = 0.1 * pi
  , pinwheelRingScale = L.map (\x -> 2 ** (x / 1)) [0 .. 0]
  , pinwheelRingRadialFreqs = [0 .. 3] -- L.map (\x -> x / 8 * pi) [0, 6, 8, 10]
  , pinwheelRingAngularFreqs = [-3 .. 3] --[-7, -6 ..- 4] L.++ [4 .. 7]
  , pinwheelRingRadius = [4..9]
  }
invariantScatteringFilterParamsFunc rows cols PinwheelBlobFilterType =
  PinwheelBlobParams
  { pinwheelBlobRows = rows
  , pinwheelBlobCols = cols
  , pinwheelBlobGaussianScale = 2 * pi
  , pinwheelBlobScale = [1]
  , pinwheelBlobFreqs = 0.5 * pi
  , pinwheelBlobOrientation = [0,10 .. 360 - 10]
  , pinwheelBlobThetaShift = [0,32 .. 127 - 32] -- [0,127]
  , pinwheelBlobRadiusShift = [22, 24, 26, 28] -- [0,32]
  }
invariantScatteringFilterParamsFunc rows cols InverseGaussianPinwheelFilterType =
  InverseGaussianPinwheelParams
  { getInverseGaussianPinwheelRows = rows
  , getInverseGaussianPinwheelCols = cols
  , getInverseGaussianPinwheelScale = [8]
  , getInverseGaussianPinwheelRadialFreq = [0 .. 9]
  , getInverseGaussianPinwheelAngularFreq = [-9 .. 9]
  }    

{-# INLINE getFilterListConvolutionFunc #-}

getFilterListConvolutionFunc
  :: PolarSeparableFilter PolarSeparableFilterConvolution
  -> [VS.Vector (Complex Double)]
getFilterListConvolutionFunc (PolarSeparableFilter _ (FourierMellinFilterConvolution xs)) =
  L.concat xs
getFilterListConvolutionFunc (PolarSeparableFilter _ (GaussianPinwheelFilterConvolution xs)) =
  L.concatMap L.concat xs
getFilterListConvolutionFunc (PolarSeparableFilter _ (PinwheelRingFilterConvolution xs)) =
  L.concatMap L.concat xs
getFilterListConvolutionFunc (PolarSeparableFilter _ (PinwheelFanFilterConvolution xs)) =
  L.concatMap L.concat xs
getFilterListConvolutionFunc (PolarSeparableFilter _ (PinwheelBlobFilterConvolution xs)) =
  L.concat xs
