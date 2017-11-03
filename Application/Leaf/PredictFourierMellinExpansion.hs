import           Application.Leaf.ArgsParser      as AP
import           Application.Leaf.Conduit
import           Classifier.LibLinear
import           Control.Monad                    as M
import           Control.Monad.Trans.Resource
import           CV.Array.LabeledArray
import           CV.Filter.FourierMellinTransform
import           CV.Statistics.KMeans
import           CV.Utility.FFT
import           CV.Utility.Parallel              as Par
import           Data.Array.Repa
import           Data.Binary
import           Data.Complex
import           Data.Conduit
import           Data.Conduit.Binary              as CB
import           Data.Conduit.List                as CL
import           Data.List                        as L
import           Data.Vector.Unboxed              as VU
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
        , Par.batchSize = AP.batchSize params
        }
      filter =
        makeFilterExpansion
          filterParams
          (div (imageSize params) 2)
          (div (imageSize params) 2) :: FourierMellinTransformExpansion
  runResourceT $
    CB.sourceFile (inputFile params) $$ readLabeledImagebinaryConduit =$=
    filterExpansionConduit parallelParams filter =$=
    featureConduit =$=
    predict (modelName params) ((modelName params) L.++ ".out")
