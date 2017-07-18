import           Application.CIFAR10.ArgsParser as AP
import           Application.Leaf.Conduit
import           Classifier.LibLinear
import           Control.Monad                  as M
import           Control.Monad.Trans.Resource
import           CV.Array.LabeledArray
import           CV.Filter.GaussianFilter
import           CV.Filter.PinwheelWavelet
import           CV.IO.ImageIO
import           CV.Statistics.KMeans
import           CV.Utility.FFT
import           CV.Utility.Parallel            as Par
import           Data.Binary
import           Data.Conduit
import           Data.Conduit.Binary            as CB
import           Data.Conduit.List              as CL
import           Data.List                      as L
import           Data.Vector                    as V
import           Data.Vector.Unboxed            as VU
import           System.Environment

main = do
  args <- getArgs
  params <- parseArgs args
  filterParams <-
    fmap (\x -> read x :: PinwheelWaveletParams) . readFile $
    (paramsFileName params)
  kmeansModel <- decodeFile (kmeansFile params)
  let parallelParams =
        ParallelParams
        { Par.numThread = AP.numThread params
        , Par.batchSize = AP.batchSize params
        }
      gFilterParams =
        GaussianFilterParams
          (gaussianScale params)
          (imageSize params)
          (imageSize params)
      fftwWisdom = FFTWWisdomPath (fftwWisdomPath params)
  fftw <- initializefftw fftwWisdom
  filters <- makeFilterConvolution fftw filterParams Normal :: IO PinwheelWaveletConvolution
  gFilters <- makeFilterConvolution fftw gFilterParams Normal :: IO GaussianFilterConvolution
  featurePtr <-
    runResourceT $
    imagePathSource (inputFile params) $$ readImageConduit True =$=
    mergeSource (labelSource (labelFile params)) =$=
    filterConduit' parallelParams fftw [filters] gFilters False (stride params) =$=
    kmeansConduit parallelParams kmeansModel =$=
    featurePtrConduitP parallelParams =$=
    CL.take (numGMMExample params)
  let trainParams =
        TrainParams
        { trainSolver = L2R_L2LOSS_SVC_DUAL
        , trainC = (c params)
        , trainNumExamples = L.length featurePtr
        , trainFeatureIndexMax =
          V.length (center kmeansModel) *
          VU.length (V.head . center $ kmeansModel)
        , trainModel = (modelName params)
        }
      (labels, features) = L.unzip featurePtr
  print trainParams
  findParameterC trainParams labels features
