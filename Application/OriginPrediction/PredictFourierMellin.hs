import           Application.Leaf.ArgsParser          as AP
import           Application.Leaf.Conduit             (featurePtrConduitP)
import           Application.OriginPrediction.Conduit
import           Classifier.LibLinear
import           Control.Monad                        as M
import           Control.Monad.Trans.Resource
import           CV.Array.LabeledArray
import           CV.Filter.FourierMellinTransform
import           CV.Utility.FFT
import           CV.Utility.Parallel                  as Par
import           Data.Array.Repa
import           Data.Binary
import           Data.Complex
import           Data.Conduit
import           Data.Conduit.Binary                  as CB
import           Data.Conduit.List                    as CL
import           Data.List                            as L
import           Data.Vector.Unboxed                  as VU
import           System.Environment

main = do
  args <- getArgs
  params <- parseArgs args
  print params
  filterParams <-
    fmap (\x -> read x :: FourierMellinTransformParamsGrid) . readFile $
    (paramsFileName params)
  let parallelParams =
        ParallelParams
        { Par.numThread = AP.numThread params
        , Par.batchSize = AP.numThread params -- AP.batchSize params
        }
      n = 10
      len = 16
      fftwWisdom = FFTWWisdomPath (fftwWisdomPath params)
  fftw <- initializefftw fftwWisdom
  filters <-
    makeFilterConvolution fftw filterParams Normal :: IO FourierMellinTransformConvolution
  runResourceT $
    CB.sourceFile (inputFile params) $$ readLabeledImagebinaryConduit =$=
    skipConduit n =$=
    filterConduit parallelParams fftw [filters] =$=
    getOriginsConduit parallelParams (modelName params) =$=
    mergeSource
      (CB.sourceFile (inputFile params) =$= readLabeledImagebinaryConduit =$=
       skipConduit n) =$=
    drawConduit parallelParams =$=
    mergeSource (sourceList [1 ..]) =$=
    plotSink "Rescaled_FMT"
