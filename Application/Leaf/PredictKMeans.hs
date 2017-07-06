import           Application.Leaf.ArgsParser                  as AP
import           Application.Leaf.Conduit
import           Classifier.LibLinear
import           Control.Monad                                as M
import           Control.Monad.Trans.Resource
import           CV.Array.LabeledArray
import           CV.Filter.FourierMellinTransform
import           CV.Filter.GaussianFilter
import           CV.Statistics.KMeans
import           CV.Utility.FFT
import           CV.Utility.Parallel                          as Par
import           Data.Binary
import           Data.Conduit
import           Data.Conduit.Binary                          as CB
import           Data.Conduit.List                            as CL
import           Data.List                                    as L
import           Data.Vector                                  as V
import           System.Environment

main = do
  args <- getArgs
  params <- parseArgs args
  filterParams <-
    fmap (\x -> read x :: FourierMellinTransformParamsGrid) . readFile $
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
  filters <- makeFilterConvolution fftw filterParams Normal :: IO FourierMellinTransformConvolution
  gFilters <- makeFilterConvolution fftw gFilterParams Normal :: IO GaussianFilterConvolution
  runResourceT $
    CB.sourceFile (inputFile params) $$ readLabeledImagebinaryConduit =$=
    filterConduit parallelParams fftw [filters] gFilters False (stride params) =$=
    kmeansConduit parallelParams kmeansModel =$=
    featureConduit =$=
    predict (modelName params) ((modelName params) L.++ ".out")
