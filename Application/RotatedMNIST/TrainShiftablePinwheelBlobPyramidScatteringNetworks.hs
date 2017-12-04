import           Application.RotatedMNIST.ArgsParser as AP
import           Application.RotatedMNIST.Conduit
import           Classifier.LibLinear
import           Control.Arrow
import           Control.Monad                       as M
import           Control.Monad.Trans.Resource
import           CV.Array.LabeledArray
import           CV.Filter.FourierMellinTransform
import           CV.Filter.ShiftablePinwheelPyramid
import           CV.Statistics.KMeans
import           CV.Statistics.PCA
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
  kmeansModels <- decodeFile (kmeansFile params)
  pcaMat <- decodeFile (pcaFile params)
  let parallelParams =
        ParallelParams
        { Par.numThread = AP.numThread params
        , Par.batchSize = AP.batchSize params
        }
      filters =
        generateShiftablePinwheelBlobPyramidScatteringNetworksFilters
          filterParams
  importFFTWWisdom (fftwWisdomPath params)
  originPredictorParams <-
    fmap (\x -> read x :: FourierMellinTransformParamsGrid) . readFile $
    (originPredictorParamsFileName params)
  (plan0, originPredictorFilters) <-
    makeFilterConvolution getEmptyPlan originPredictorParams Normal :: IO (DFTPlan, FourierMellinTransformConvolution)
  plan <- shiftablePinwheelBlobPyramidScatteringNetworksPlan plan0 filters
  (y:_) <-
    runResourceT $
    CB.sourceFile (inputFile params) $$ readLabeledImagebinaryConduit =$=
    getOriginsConduit
      parallelParams
      plan
      [originPredictorFilters]
      (originModelName params) =$=
    logpolarImageConduit
      parallelParams
      (shiftablePinwheelBlobPyramidNumTheta filterParams)
      (shiftablePinwheelBlobPyramidNumLogR filterParams)
      (radius params)
      (logpolarFlag params)
      (numGrid params) =$=
    shiftablePinwheelBlobPyramidScatteringNetworksConduit
      parallelParams
      plan
      (numScatteringLayer params)
      (stride params)
      (shiftablePinwheelBlobPyramidNumLayers filterParams)
      (shiftablePinwheelBlobPyramidK filterParams)
      filters
      Nothing =$=
    -- CL.map (second $ L.head . L.concat) =$=
    kmeansConduit parallelParams kmeansModels =$=
    pcaConduitSingleModel parallelParams pcaMat =$=
    CL.take 1
  featurePtr <-
    runResourceT $
    CB.sourceFile (inputFile params) $$ readLabeledImagebinaryConduit =$=
    -- remove9Conduit =$=
    getOriginsConduit
      parallelParams
      plan
      [originPredictorFilters]
      (originModelName params) =$=
    logpolarImageConduit
      parallelParams
      (shiftablePinwheelBlobPyramidNumTheta filterParams)
      (shiftablePinwheelBlobPyramidNumLogR filterParams)
      (radius params)
      (logpolarFlag params)
      (numGrid params) =$=
    shiftablePinwheelBlobPyramidScatteringNetworksConduit
      parallelParams
      plan
      (numScatteringLayer params)
      (stride params)
      (shiftablePinwheelBlobPyramidNumLayers filterParams)
      (shiftablePinwheelBlobPyramidK filterParams)
      filters
      Nothing =$=
    -- CL.map (second $ L.head . L.concat) =$=
    kmeansConduit parallelParams kmeansModels =$=
    pcaConduitSingleModel parallelParams pcaMat =$=
    featurePtrConduit =$=
    CL.take (numGMMExample params)
  let trainParams =
        TrainParams
        { trainSolver = L2R_L2LOSS_SVC_DUAL
        , trainC = (c params)
        , trainNumExamples = L.length featurePtr
        , trainFeatureIndexMax = VU.length . snd $ y
        , trainModel = (modelName params)
        }
      (labels, features) = L.unzip featurePtr
  print trainParams
  train trainParams labels features
