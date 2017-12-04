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
import           CV.Statistics.KMeans

main = do
  args <- getArgs
  params <- parseArgs args
  print params
  let parallelParams =
        ParallelParams
        { Par.numThread = AP.numThread params
        , Par.batchSize = AP.batchSize params
        }
  xs' <-
    runResourceT $
    CB.sourceFile (inputFile params) $$ readLabeledImagebinaryConduit =$=
    CL.map
      (\(LabeledArray label arr) ->
         (fromIntegral label, [[rescaleVector . toUnboxed $ arr]])) =$=
    CL.take (numGMMExample params)
  let (_, ys') = L.unzip xs'
  -- kmeansModel <-
  --   M.mapM
  --     (kmeans parallelParams (numGaussian params) (kmeansFile params) 0.005)
  --     (L.map L.concat . L.transpose $ ys')
  -- encodeFile (kmeansFile params) kmeansModel
  (y:_) <-
    runResourceT $
    CB.sourceFile (inputFile params) $$ readLabeledImagebinaryConduit =$=
    CL.map
      (\(LabeledArray label arr) ->
         (fromIntegral label, rescaleVector . toUnboxed $ arr)) =$=
    -- kmeansConduit parallelParams kmeansModel =$=
    CL.take 1
  featurePtr <-
    runResourceT $
    CB.sourceFile (inputFile params) $$ readLabeledImagebinaryConduit =$=
    CL.map
      (\(LabeledArray label arr) ->
         (fromIntegral label, rescaleVector . toUnboxed $ arr)) =$=
    -- kmeansConduit parallelParams kmeansModel =$=
    featurePtrConduit =$=
    -- svmFeatureConduit parallelParams =$=
    CL.take (numGMMExample params)
  let (xs, ys) = L.unzip featurePtr
      -- trainParams =
      --   TrainParams
      --   { svmType = C_SVC
      --   , kernelType = RBF
      --   , SVM.modelName = AP.modelName params
      --   , numFeature = (imageSize params) ^ (2 :: Int)
      --   , SVM.c = AP.c params
      --   , eps = 0.001
      --   , nu = 0.5
        -- }
      trainParams =
        TrainParams
        { trainSolver = L2R_L2LOSS_SVC_DUAL
        , trainC = (c params)
        , trainNumExamples = L.length featurePtr
        , trainFeatureIndexMax = VU.length . snd $ y
        , trainModel = (modelName params)
        }
  train trainParams xs ys
  -- oneVsRestTrain trainParams xs ys
