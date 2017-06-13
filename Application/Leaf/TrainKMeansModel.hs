import           Application.Leaf.ArgsParser                  as AP
import           Application.Leaf.Conduit
import           Application.MultiDimensionalGMM.FisherKernel
import           Application.MultiDimensionalGMM.GMM
import           Classifier.LibLinear
import           Control.Concurrent.MVar                      (newMVar)
import           Control.Monad                                as M
import           Control.Monad.Trans.Resource
import           CV.Array.LabeledArray
import           CV.Filter.GaussianFilter                     as Gaussian
import           CV.Statistics.KMeans
import           CV.Utility.FFT
import           CV.Utility.Parallel                          as Par
import           CV.V4Filter
import           CV.V4FilterConvolution
import           Data.Array.Repa                              as R
import           Data.Binary
import           Data.Complex
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
      m = 10
      filterParams =
        V4SeparableFilterParamsAxisConvolution
        { v4SeparableFilterParamsAxisConvolutionSeparableFilterRows =
          imageSize params
        , v4SeparableFilterParamsAxisConvolutionSeparableFilterCols =
          imageSize params
        , v4SeparableFilterParamsAxisConvolutionPolarSeparablePolarFactor = 1
        , v4SeparableFilterParamsAxisConvolutionPolarSeparableScale = [56]
        , v4SeparableFilterParamsAxisConvolutionPolarSeparableFreq = [1 .. 16]
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
      gFilterParams =
        L.map
          (\s -> GaussianFilterParams s (imageSize params) (imageSize params)) .
        gaussianScale $
        params
      filters = generateV4SeparableFilterAxisConvolution filterParams
      fftwWisdom = FFTWWisdomPath (fftwWisdomPath params)
  writeFile (paramsFileName params) . show $ filterParams
  fftwInit <- initializefftw FFTWWisdomNull
  imgs <-
    runResourceT $
    CB.sourceFile (inputFile params) $$ readLabeledImagebinaryConduit =$=
    CL.map (\(LabeledArray _ arr) -> toUnboxed arr) =$=
    CL.take 1
  generateWisdom
    fftwInit
    (fftwWisdomPath params)
    (imageSize params)
    (imageSize params) .
    VU.convert . VU.map (:+ 0) . L.head $
    imgs
  fftw <- initializefftw fftwWisdom
  filtersF <-
    M.mapM (fourierTransformFilter fftw (imageSize params, imageSize params)) filters
  gFilters <- M.mapM (fmap getFilter . Gaussian.makeFilter fftw) gFilterParams
  xs <-
    runResourceT $
    CB.sourceFile (inputFile params) $$ readLabeledImagebinaryConduit =$=
    applyV4SeparableFilterConvolutionLabeledArrayConduit
      fftw
      parallelParams
      (stride params)
      gFilters
      filtersF =$=
    magnitudeConduit parallelParams =$=
    CL.take (numGMMExample params)
  let (ls, ys) = L.unzip xs
  kmeansModel <- kmeans parallelParams (numGaussian params) (L.concat ys)
  encodeFile (kmeansFile params) kmeansModel
