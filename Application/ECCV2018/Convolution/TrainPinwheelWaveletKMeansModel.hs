import           Application.ECCV2018.ArgsParser          as AP
import           Application.ECCV2018.Conduit
import           Application.ECCV2018.Convolution.Conduit
import           Control.Arrow
import           Control.Monad                            as M
import           Control.Monad.Trans.Resource
import           CV.Array.Image
import           CV.Array.LabeledArray
import           CV.Filter.PinwheelWavelet
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
      filterParams =
        case filterType params of
          "PinwheelFan" ->
            PinwheelWaveletFanParams
              PinwheelFanParams
              { pinwheelFanRows = rows
              , pinwheelFanCols = cols
              , pinwheelFanGaussianScale = 0.1 * pi
              , pinwheelFanScale = L.map (\x -> 2 ** (x / 1)) [0 .. 1]
              , pinwheelFanRadialFreqs = [0 .. 7] -- L.map (\x -> x / 8 * pi) [0, 6, 8, 10]
              , pinwheelFanAngularFreqs = [0 .. 7]
              , pinwheelFanTheta = L.map (* (2 * pi)) [0.05,0.1 .. 1]
              }
          "PinwheelRing" ->
            PinwheelWaveletRingParams
              PinwheelRingParams
              { pinwheelRingRows = rows
              , pinwheelRingCols = cols
              , pinwheelRingGaussianScale = 0.15 * pi
              , pinwheelRingScale = L.map (\x -> 2 ** (x / 4)) [0 .. 2]
              , pinwheelRingRadialFreqs = [0 .. 7] -- L.map (\x -> x / 8 * pi) [0, 6, 8, 10]
              , pinwheelRingAngularFreqs = [0 .. 7]
              , pinwheelRingRadius = [4,6,8]
              }
          "PinwheelBlob" ->
            PinwheelWaveletBlobParams
              PinwheelBlobParams
              { pinwheelBlobRows = rows
              , pinwheelBlobCols = cols
              , pinwheelBlobGaussianScale = 2 * pi
              , pinwheelBlobScale = [1]
              , pinwheelBlobFreqs = 0.5 * pi
              , pinwheelBlobOrientation = [0,10 .. 360 - 10]
              , pinwheelBlobThetaShift = [0,32 .. 127 - 32] -- [0,127]
              , pinwheelBlobRadiusShift = [22,24,26,28] -- [0,32]
              }
          _ -> error "TrainPinwheelWaveletKMeansModel: filter name error."
  print filterParams
  (plan, filters) <-
    makePinwheelWaveletFilterConvolution getEmptyPlan filterParams Normal
  writeFile (paramsFileName params) . show $ filterParams
  runResourceT $
    CB.sourceFile (inputFile params) $$ readLabeledImagebinaryConduit =$=
    pinwheelWaveletConvolutionConduit parallelParams plan filters =$=
    (if invariantFeatureFlag params
       then invariantFeatureExtractionConduit parallelParams (stride params)
       else nonInvariantFeatureExtractionConduit parallelParams (stride params)) =$=
    kmeansSink
      parallelParams
      (numGMMExample params)
      (numGaussian params)
      (kmeansFile params)
      (threshold params)
