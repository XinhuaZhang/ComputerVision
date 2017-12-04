import           Application.RotatedMNIST.ArgsParser as AP
import           Application.RotatedMNIST.Conduit
-- import           Classifier.LibSVM as SVM
import           Classifier.LibLinear
import           Control.Arrow
import           Control.Monad                       as M
import           Control.Monad.Trans.Resource
import           CV.Array.LabeledArray
import           CV.Utility.Parallel                 as Par
import           Data.Array.Repa                     as R
import           Data.Binary
import           Data.Conduit
import           Data.Conduit.Binary                 as CB
import           Data.Conduit.List                   as CL
import           Data.List                           as L
import           Data.Vector                         as V
import           Data.Vector.Unboxed                 as VU
import           System.Environment

main = do
  args <- getArgs
  params <- parseArgs args
  print params
  -- kmeansModels <- decodeFile (kmeansFile params)
  let parallelParams =
        ParallelParams
        { Par.numThread = AP.numThread params
        , Par.batchSize = AP.batchSize params
        }
  runResourceT $
    CB.sourceFile (inputFile params) $$ readLabeledImagebinaryConduit =$=
    CL.map
      (\(LabeledArray label arr) ->
         (fromIntegral label, rescaleVector . toUnboxed $ arr)) =$=
    -- kmeansConduit parallelParams kmeansModels =$=
    featureConduit =$=
    -- svmFeatureConduit parallelParams =$=
    predict (AP.modelName params) (AP.modelName params L.++ ".out")
    -- oneVsRestPredict
    --   (AP.modelName params)
    --   (AP.modelName params L.++ ".out")
    --   (0, 9)
