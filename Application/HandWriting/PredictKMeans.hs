import           Application.HandWriting.ArgsParser           as AP
import           Application.HandWriting.Conduit
import           Application.HandWriting.IO
import           Application.Leaf.Conduit                     (kmeansConduit)
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
import           CV.V4FilterConvolution                       hiding (applyV4SeparableFilterConvolutionLabeledArrayConduit)
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
  filterParams <-
    fmap (\x -> read x :: V4SeparableFilterParamsAxisConvolution) . readFile $
    (paramsFileName params)
  kmeansModel <- decodeFile (kmeansFile params)
  let parallelParams =
        ParallelParams
        { Par.numThread = AP.numThread params
        , Par.batchSize = AP.batchSize params
        }
      gFilterParams =
        L.map
          (\s -> GaussianFilterParams s (imageSize params) (imageSize params)) .
        gaussianScale $
        params
      filters = generateV4SeparableFilterAxisConvolution filterParams
      fftwWisdom = FFTWWisdomPath (fftwWisdomPath params)
  fftw <- initializefftw fftwWisdom
  filtersF <-
    M.mapM (fourierTransformFilter fftw (imageSize params, imageSize params)) filters
  gFilters <- M.mapM (fmap getFilter . Gaussian.makeFilter fftw) gFilterParams
  runResourceT $
    CB.sourceFile (inputFile params) $$ sparseOfflineCharacterConduit =$=
    applyV4SeparableFilterConvolutionLabeledArrayConduit
      fftw
      parallelParams
      (stride params)
      gFilters
      filtersF =$=
    kmeansConduit parallelParams kmeansModel =$=
    featureConduitP parallelParams =$=
    predict (modelName params) ((modelName params) L.++ ".out")
