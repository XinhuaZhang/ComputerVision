import           Application.OriginPrediction.Conduit
import           Application.RotatedMNIST.ArgsParser  as AP
import           Classifier.LibLinear
import           Control.Monad                        as M
import           Control.Monad.Trans.Resource
import           CV.Array.LabeledArray
import           CV.Filter.FourierMellinTransform
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
  importFFTWWisdom (fftwWisdomPath params)
  (plan, filters) <-
    makeFilterConvolution getEmptyPlan filterParams Normal :: IO (DFTPlan, FourierMellinTransformConvolution)
  runResourceT $
    CB.sourceFile (inputFile params) $$ readLabeledImagebinaryConduit =$=
    skipConduit n =$=
    filterConduit parallelParams plan [filters] =$=
    getOriginsConduit parallelParams (originModelName params) =$=
    mergeSource
      (CB.sourceFile (inputFile params) =$= readLabeledImagebinaryConduit =$=
       skipConduit n) =$=
    drawConduit parallelParams =$=
    mergeSource (sourceList [1 ..]) =$=
    plotSink "FMT"
