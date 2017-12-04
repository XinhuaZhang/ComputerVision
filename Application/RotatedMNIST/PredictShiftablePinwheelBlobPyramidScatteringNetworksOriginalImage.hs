import           Application.RotatedMNIST.ArgsParser as AP
import           Application.RotatedMNIST.Conduit
-- import           Classifier.LibLinear
import           Classifier.LibSVM as SVM
import           Control.Arrow
import           Control.Monad                       as M
import           Control.Monad.Trans.Resource
import           CV.Array.LabeledArray
import           CV.Filter.FourierMellinTransform
import           CV.Filter.ShiftablePinwheelPyramid
import           CV.Statistics.KMeans
import           CV.Utility.Parallel                 as Par
import           Data.Array.Unboxed
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
  filterParams <-
    fmap (\x -> read x :: ShiftablePinwheelBlobPyramidParams) . readFile $
    (paramsFileName params)
  -- kmeansModels <- decodeFile (kmeansFile params)
  let parallelParams =
        ParallelParams
        { Par.numThread = AP.numThread params
        , Par.batchSize = AP.batchSize params
        }
      filters =
        generateShiftablePinwheelBlobPyramidScatteringNetworksFilters
          filterParams
  importFFTWWisdom (fftwWisdomPath params)
  plan <-
    shiftablePinwheelBlobPyramidScatteringNetworksPlan getEmptyPlan filters
  runResourceT $
    CB.sourceFile (inputFile params) $$ readLabeledImagebinaryConduit =$=
    imageConduit parallelParams =$=
    shiftablePinwheelBlobPyramidScatteringNetworksConduit
      parallelParams
      plan
      (numScatteringLayer params)
      (stride params)
      (shiftablePinwheelBlobPyramidNumLayers filterParams)
      (shiftablePinwheelBlobPyramidK filterParams)
      filters
      Nothing =$=
    concatConduit parallelParams =$=
    CL.map (second $ rescaleVector .  L.head . L.concat) =$=
    -- kmeansConduit parallelParams kmeansModels =$=
    -- featureConduit =$=
    svmFeatureConduit parallelParams =$=
    predict (AP.modelName params) ((AP.modelName params) L.++ ".out")
