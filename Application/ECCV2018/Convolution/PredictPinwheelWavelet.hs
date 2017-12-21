import           Application.ECCV2018.ArgsParser          as AP
import           Application.ECCV2018.Conduit
import           Application.ECCV2018.Convolution.Conduit
import           Classifier.LibLinear
import           Control.Monad                            as M
import           Control.Monad.Trans.Resource
import           CV.Array.LabeledArray
import           CV.Filter.PinwheelWavelet
import           CV.Utility.Parallel                      as Par
import           Data.Array.Repa
import           Data.Binary
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
  filterParams <-
    fmap (\x -> read x :: PinwheelWaveletParams) . readFile $
    (paramsFileName params)
  kmeansModel <- decodeFile (kmeansFile params)
  let parallelParams =
        ParallelParams
        { Par.numThread = AP.numThread params
        , Par.batchSize = AP.batchSize params
        }
  (plan, filters) <-
    makeFilterConvolution getEmptyPlan filterParams Normal :: IO (DFTPlan, PinwheelWaveletConvolution)
  writeFile (paramsFileName params) . show $ filterParams
  runResourceT $
    CB.sourceFile (inputFile params) $$ readLabeledImagebinaryConduit =$=
    pinwheelWaveletConvolutionConduit parallelParams plan filters =$=
    predictFeatureConduit parallelParams (stride params) =$=
    kmeansPredictConduit parallelParams kmeansModel =$=
    featurePredictConduit =$=
    predictVote (modelName params) ((modelName params) L.++ ".out")
