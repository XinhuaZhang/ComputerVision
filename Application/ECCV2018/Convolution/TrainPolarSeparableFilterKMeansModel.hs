import           Application.ECCV2018.ArgsParser          as AP
import           Application.ECCV2018.Conduit
import           Application.ECCV2018.Convolution.Conduit
import           Control.Arrow
import           Control.Monad                            as M
import           Control.Monad.Trans.Resource
import           CV.Array.Image
import           CV.Array.LabeledArray
import           CV.Filter.PolarSeparableFilter
import           CV.Utility.Parallel                      as Par
import           Data.Array.Repa
import           Data.Binary
import           Data.Complex
import           Data.Conduit
import           Data.Conduit.Binary                      as CB
import           Data.Conduit.List                        as CL
import           Data.List                                as L
import           Data.Vector.Unboxed                      as VU
import           System.Environment

{-# INLINE filterParamsFunc #-}

filterParamsFunc :: Int
                 -> Int
                 -> PolarSeparableFilterType
                 -> PolarSeparableFilterParams
filterParamsFunc rows cols FourierMellinFilterType =
  FourierMellinTransformParams
  { getFourierMellinTransformRows = rows
  , getFourierMellinTransformCols = cols
  , getFourierMellinTransformRadialFreq = [0 .. 7]
  , getFourierMellinTransformAngularFreq = [0 .. 7]
  }
filterParamsFunc rows cols GaussianPinwheelFilterType =
  GaussianPinwheelParams
  { getGaussianPinwheelRows = rows
  , getGaussianPinwheelCols = cols
  , getGaussianPinwheelScale = L.map (* pi) [0.3]
  , getGaussianPinwheelRadialFreq = [0 .. 7]
  , getGaussianPinwheelAngularFreq = [-7 .. 7]
  }
filterParamsFunc rows cols PinwheelFanFilterType =
  PinwheelFanParams
  { pinwheelFanRows = rows
  , pinwheelFanCols = cols
  , pinwheelFanGaussianScale = 0.1 * pi
  , pinwheelFanScale = L.map (\x -> 2 ** (x / 1)) [0 .. 1]
  , pinwheelFanRadialFreqs = [0 .. 7] -- L.map (\x -> x / 8 * pi) [0, 6, 8, 10]
  , pinwheelFanAngularFreqs = [0 .. 7]
  , pinwheelFanTheta = L.map (* (2 * pi)) [0.05,0.1 .. 1]
  }  
filterParamsFunc rows cols PinwheelRingFilterType =
  PinwheelRingParams
  { pinwheelRingRows = rows
  , pinwheelRingCols = cols
  , pinwheelRingGaussianScale = 0.15 * pi
  , pinwheelRingScale = L.map (\x -> 2 ** (x / 4)) [0 .. 2]
  , pinwheelRingRadialFreqs = [0 .. 7] -- L.map (\x -> x / 8 * pi) [0, 6, 8, 10]
  , pinwheelRingAngularFreqs = [0 .. 7]
  , pinwheelRingRadius = [4, 6, 8]
  }
filterParamsFunc rows cols PinwheelBlobFilterType =
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

main = do
  args <- getArgs
  params <- parseArgs args
  print params
  ((LabeledArray _ img):_) <-
    runResourceT $
    CB.sourceFile (inputFile params) $$ readLabeledImagebinaryConduit =$=
    CL.take 1
  let (Z :. _ :. rows :. cols) = extent img
      parallelParams =
        ParallelParams
        { Par.numThread = AP.numThread params
        , Par.batchSize = AP.batchSize params
        }
      filterParamsList = L.map (filterParamsFunc rows cols) (filterType params)
  print filterParamsList
  (plan, filters) <-
    makePolarSeparableFilterConvolutionList getEmptyPlan filterParamsList
  writeFile (paramsFileName params) . show $ filterParamsList
  runResourceT $
    CB.sourceFile (inputFile params) $$ readLabeledImagebinaryConduit =$=
    polarSeparableFilterConvolutionConduit parallelParams plan filters =$=
    invariantFeatureExtractionConduit parallelParams (stride params) =$=
    kmeansSink
      parallelParams
      (numGMMExample params)
      (numGaussian params)
      (kmeansFile params)
      (threshold params)
  undefined
