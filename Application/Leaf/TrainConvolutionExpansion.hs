import           Application.Leaf.ArgsParser                  as AP
import           Application.Leaf.Conduit
import           Application.RecenterImage.Conduit
import           Classifier.LibLinear
import           Control.Concurrent.MVar                      (newMVar)
import           Control.Monad                                as M
import           Control.Monad.Trans.Resource
import           CV.Array.LabeledArray
import           CV.Filter.GaussianFilter                     as Gaussian
import           CV.Utility.Parallel                          as Par
import           CV.V4Filter
import           CV.V4FilterConvolution
import           Data.Array.Repa                              as R
import           Data.Binary
import           Data.Conduit
import           Data.Conduit.Binary                          as CB
import           Data.Conduit.List                            as CL
import           Data.List                                    as L
import           Data.Vector                                  as V
import           Data.Vector.Unboxed                          as VU
import           System.Environment
import           System.IO


main = do
  args <- getArgs
  params <- parseArgs args
  print params
  let parallelParams =
        ParallelParams
        { Par.numThread = AP.numThread params
        , Par.batchSize = AP.batchSize params
        }
      m = 15
      n = 16
      convolutionFilterParams =
        V4SeparableFilterParamsAxisConvolution
        { v4SeparableFilterParamsAxisConvolutionSeparableFilterRows =
          imageSize params
        , v4SeparableFilterParamsAxisConvolutionSeparableFilterCols =
          imageSize params
        , v4SeparableFilterParamsAxisConvolutionPolarSeparablePolarFactor = 1
        , v4SeparableFilterParamsAxisConvolutionPolarSeparableScale = [56]
        , v4SeparableFilterParamsAxisConvolutionPolarSeparableFreq = [1 .. n]
        , v4SeparableFilterParamsAxisConvolutionPolarSeparableAngle =
          [0,m .. 90 - m]
        , v4SeparableFilterParamsAxisConvolutionCartesianGratingScale =
          [ 2 ** (i / 2)
          | i <- [7 .. 10] ]
        , v4SeparableFilterParamsAxisConvolutionCartesianGratingFreq =
          L.take 8 [1 .. 8]
        , v4SeparableFilterParamsAxisConvolutionCartesianGratingAngle =
          [0,15 .. 360 - 15]
        , v4SeparableFilterParamsAxisConvolutionHyperbolicSeparableScale =
          [ 2 ** (i / 2)
          | i <- [7 .. 10] ]
        , v4SeparableFilterParamsAxisConvolutionHyperbolicSeparableUFreq =
          [0 .. 3]
        , v4SeparableFilterParamsAxisConvolutionHyperbolicSeparableVFreq =
          [0 .. 3]
        , v4SeparableFilterParamsAxisConvolutionHyperbolicSeparableAngle = 15
        , v4SeparableFilterParamsAxisConvolutionSeparableFilterParams = P
        }
      convolutionGFilterParams =
        L.map
          (\s -> GaussianFilterParams s (imageSize params) (imageSize params)) .
        gaussianScale $
        params
      convolutionFilters = generateV4SeparableFilterAxisConvolution convolutionFilterParams
      filterParams =
        V4SeparableFilterParamsAxis
        { v4SeparableFilterParamsAxisSeparableFilterRows = imageSize params
        , v4SeparableFilterParamsAxisSeparableFilterCols = imageSize params
        , v4SeparableFilterParamsAxisPolarSeparablePolarFactor = 1
        , v4SeparableFilterParamsAxisPolarSeparableScale = [32]
        , v4SeparableFilterParamsAxisPolarSeparableFreq = [1 .. n]
        , v4SeparableFilterParamsAxisPolarSeparableAngle = [0,m .. 90 - m]
        , v4SeparableFilterParamsAxisCartesianGratingScale =
          [ 2 ** (i / 2)
          | i <- [7 .. 10] ]
        , v4SeparableFilterParamsAxisCartesianGratingFreq = L.take 8 [1 .. 8]
        , v4SeparableFilterParamsAxisCartesianGratingAngle = [0,15 .. 360 - 15]
        , v4SeparableFilterParamsAxisHyperbolicSeparableScale =
          [ 2 ** (i / 2)
          | i <- [7 .. 10] ]
        , v4SeparableFilterParamsAxisHyperbolicSeparableUFreq = [0 .. 3]
        , v4SeparableFilterParamsAxisHyperbolicSeparableVFreq = [0 .. 3]
        , v4SeparableFilterParamsAxisHyperbolicSeparableAngle = 15
        , v4SeparableFilterParamsAxisSeparableFilterParams = P
        }
      gaussianFilterParams =
        GaussianFilterParams 32 (imageSize params) (imageSize params)
  writeFile ("Convolution_" L.++ paramsFileName params) . show $ convolutionFilterParams
  writeFile ("Expansion_" L.++ paramsFileName params) . show $ filterParams
  lock <- newMVar ()
  convolutionFiltersF <-
    M.mapM (fourierTransformFilter lock (imageSize params, imageSize params)) convolutionFilters
  gConvolutionFilters <-
    M.mapM (fmap (getFilter . fmap toUnboxed) . Gaussian.makeFilter lock) convolutionGFilterParams
  gaussianFilter <- Gaussian.makeFilter lock gaussianFilterParams
  featurePtr <-
    runResourceT $
    CB.sourceFile (inputFile params) $$ readLabeledImagebinaryConduit =$=
    applyV4SeparableFilterConvolutionLabeledArrayConduitP
      parallelParams
      (stride params)
      gConvolutionFilters
      convolutionFiltersF =$=
    applyV4SeparableFilterLabeledArrayWithCenterConduit1
      lock
      parallelParams
      gaussianFilter
      filterParams =$=
    featurePtrConduit =$=
    CL.consume
  featurePtr1 <-
    runResourceT $
    CB.sourceFile (inputFile params) $$ readLabeledImagebinaryConduit =$=
    applyV4SeparableFilterConvolutionLabeledArrayConduit
      lock
      parallelParams
      (stride params)
      gConvolutionFilters
      convolutionFiltersF =$=
    applyV4SeparableFilterLabeledArrayWithCenterConduit1
      lock
      parallelParams
      gaussianFilter
      filterParams =$=
    CL.take 1
  let trainParams =
        TrainParams
        { trainSolver = L2R_L2LOSS_SVC_DUAL
        , trainC = 512
        , trainNumExamples = L.length featurePtr
        , trainFeatureIndexMax = VU.length . snd . L.head $ featurePtr1
        , trainModel = modelName params
        }
      (labels, features) = L.unzip featurePtr
  print trainParams
  train trainParams labels features 
