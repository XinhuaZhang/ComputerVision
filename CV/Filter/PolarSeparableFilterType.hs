{-# LANGUAGE BangPatterns #-}
module CV.Filter.PolarSeparableFilterType where

import           Control.DeepSeq      (NFData, rnf)
import           Data.Complex
import           Data.Vector.Storable as VS
import           Data.Vector.Unboxed  as VU

data PolarSeparableFilterType
  = FourierMellinFilterType
  | GaussianPinwheelFilterType
  | InverseGaussianPinwheelFilterType
  | PinwheelFanFilterType
  | PinwheelRingFilterType
  | PinwheelBlobFilterType
  deriving (Show, Read)

data PolarSeparableFilterParams
  = FourierMellinTransformParams { getFourierMellinTransformRows :: !Int
                                ,  getFourierMellinTransformCols :: !Int
                                ,  getFourierMellinTransformRadialFreq :: ![Double]
                                ,  getFourierMellinTransformAngularFreq :: ![Int]}
  | GaussianPinwheelParams { getGaussianPinwheelRows        :: !Int
                          ,  getGaussianPinwheelCols        :: !Int
                          ,  getGaussianPinwheelScale       :: ![Double]
                          ,  getGaussianPinwheelRadialFreq  :: ![Int]
                          ,  getGaussianPinwheelAngularFreq :: ![Int]}
  | InverseGaussianPinwheelParams { getInverseGaussianPinwheelRows :: !Int
                                 ,  getInverseGaussianPinwheelCols :: !Int
                                 ,  getInverseGaussianPinwheelScale :: ![Double]
                                 ,  getInverseGaussianPinwheelRadialFreq :: ![Int]
                                 ,  getInverseGaussianPinwheelAngularFreq :: ![Int]}
  | PinwheelFanParams { pinwheelFanRows          :: !Int
                     ,  pinwheelFanCols          :: !Int
                     ,  pinwheelFanGaussianScale :: !Double
                     ,  pinwheelFanScale         :: ![Double]
                     ,  pinwheelFanRadialFreqs   :: ![Double]
                     ,  pinwheelFanAngularFreqs  :: ![Int]
                     ,  pinwheelFanTheta         :: ![Double]}
  | PinwheelRingParams { pinwheelRingRows          :: !Int
                      ,  pinwheelRingCols          :: !Int
                      ,  pinwheelRingGaussianScale :: !Double
                      ,  pinwheelRingScale         :: ![Double]
                      ,  pinwheelRingRadialFreqs   :: ![Double]
                      ,  pinwheelRingAngularFreqs  :: ![Int]
                      ,  pinwheelRingRadius        :: ![Double]}
  | PinwheelBlobParams { pinwheelBlobRows          :: !Int
                      ,  pinwheelBlobCols          :: !Int
                      ,  pinwheelBlobGaussianScale :: !Double
                      ,  pinwheelBlobScale         :: ![Double]
                      ,  pinwheelBlobFreqs         :: !Double
                      ,  pinwheelBlobOrientation   :: ![Double]
                      ,  pinwheelBlobThetaShift    :: ![Double]
                      ,  pinwheelBlobRadiusShift   :: ![Double]}
  deriving (Show, Read)

instance NFData PolarSeparableFilterParams where
  rnf !_ = ()

data PolarSeparableFilter a =
  PolarSeparableFilter PolarSeparableFilterParams
                       a

data PolarSeparableFilterExpansion
  = FourierMellinFilterExpansion  [[VU.Vector (Complex Double)]]
  | GaussianPinwheelFilterExpansion [[[VU.Vector (Complex Double)]]]
  | InverseGaussianPinwheelFilterExpansion [[[VU.Vector (Complex Double)]]]
  | PinwheelFanFilterExpansion [[[VU.Vector (Complex Double)]]]
  | PinwheelRingFilterExpansion [[[VU.Vector (Complex Double)]]]
  | PinwheelBlobFilterExpansion [[VU.Vector (Complex Double)]]

data PolarSeparableFilterConvolution
  = FourierMellinFilterConvolution [[VS.Vector (Complex Double)]]
  | GaussianPinwheelFilterConvolution [[[VS.Vector (Complex Double)]]]
  | InverseGaussianPinwheelFilterConvolution [[[VS.Vector (Complex Double)]]]
  | PinwheelFanFilterConvolution [[[VS.Vector (Complex Double)]]]
  | PinwheelRingFilterConvolution [[[VS.Vector (Complex Double)]]]
  | PinwheelBlobFilterConvolution [[VS.Vector (Complex Double)]]
