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
  (y:_) <-
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
    CL.take 1
  featurePtr <-
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
    CL.map (second $ rescaleVector . L.head . L.concat) =$=
    -- kmeansConduit parallelParams kmeansModels =$=
    -- featurePtrConduit =$=
    svmFeatureConduit parallelParams =$=
    CL.take (numGMMExample params)
  let -- trainParams =
      --   TrainParams
      --   { trainSolver = L2R_L2LOSS_SVC_DUAL
      --   , trainC = (c params)
      --   , trainNumExamples = L.length featurePtr
      --   , trainFeatureIndexMax = VU.length . snd $ y
      --   , trainModel = (modelName params)
      --   }
      trainParams =
        TrainParams
        { svmType = C_SVC
        , kernelType = RBF
        , SVM.modelName = AP.modelName params
        , numFeature = VU.length . snd $ y
        , SVM.c = AP.c params
        , eps = 0.001
        , nu = 0.5
        }
      (labels, features) = L.unzip featurePtr
  print trainParams
  train trainParams labels features
