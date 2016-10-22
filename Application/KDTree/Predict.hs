module Main where

import           Application.KDTree.ArgsParser      as Parser
import           Application.KDTree.Conduit
import           Application.KDTree.KDTree
import           Application.KDTree.KdTreeStatic    as KDT
import           Classifier.LibSVM                  as SVM
import           CV.CUDA.Context
import           CV.CUDA.DataType
import           CV.Feature.PolarSeparable
import           CV.Filter
import           CV.Filter.FilterStats
import           CV.Filter.PolarSeparableFilter
import           CV.IO.ImageIO
import           CV.Utility.Parallel                as Parallel
import           Data.Array.Accelerate              as A
import           Data.Array.Accelerate.Data.Complex as A
import           Data.Binary
import           Data.Conduit
import           Data.Conduit.List                  as CL
import qualified Data.Set                           as S
import           GHC.Float
import           Prelude                            as P
import           System.Environment

main = do
  args <- getArgs
  if P.null args
    then error "run with --help to see options."
    else return ()
  params <- parseArgs args
  let parallelParams =
        ParallelParams
        { Parallel.numThread = Parser.numThread params
        , Parallel.batchSize = Parser.batchSize params
        }
      filterParams =
        PolarSeparableFilterParams
        { getRadius = 128
        , getScale = S.fromDistinctAscList [8]
        , getRadialFreq = S.fromDistinctAscList [0 .. 3]
        , getAngularFreq = S.fromDistinctAscList [0 .. 3]
        , getName = Pinwheels
        }
  ctx <- initializeGPUCtx (Option $ gpuId params)
  print params
  trees <- readKdTree (treeFile params) 
  labels <- readLabelFile (labelFile params)
  filterStats <-
    readFilterStats $
    "../FilterStatistics/" P.++ "N" P.++ (show $ getFilterNum filterParams) P.++
    "S" P.++
    (show $ S.toList $ getScale filterParams) P.++
    "MeanVar.data"
  let (labelMin, labelMax) =
        (P.round $ P.minimum labels, P.round $ P.maximum labels)
  case gpuDataType params of
    GPUFloat ->
      let filters =
            makeFilter filterParams :: PolarSeparableFilter (Acc (A.Array DIM3 (A.Complex Float)))
          meanArr =
            A.replicate
              (A.lift $
               Z :. (getRadius filterParams * 2) :. (getRadius filterParams * 2) :.
               All) .
            A.use . A.fromList (Z :. P.length (mean filterStats)) . P.map double2Float $
            (mean filterStats)
          varArr =
            A.replicate
              (A.lift $
               Z :. (getRadius filterParams * 2) :. (getRadius filterParams * 2) :.
               All) .
            A.use . A.fromList (Z :. P.length (var filterStats)) . P.map double2Float $
            (var filterStats)
      in imagePathSource (inputFile params) $$ grayImageConduit =$= grayImage2FloatArrayConduit =$=
         magnitudeConduitFloat
           parallelParams
           ctx
           filters
           (downsampleFactor params)
           meanArr
           varArr =$=
         buildTreeConduit parallelParams =$=
         libSVMPredictConduit parallelParams trees (radius params) (sampleRate params) =$=
         mergeSource (labelSource $ labelFile params) =$
         oneVsRestPredict
           (Parser.modelName params)
           ((Parser.modelName params) P.++ ".out")
           (labelMin, labelMax)
    GPUDouble ->
      let filters =
            makeFilter filterParams :: PolarSeparableFilter (Acc (A.Array DIM3 (A.Complex Double)))
          meanArr =
            A.replicate
              (A.lift $
               Z :. (getRadius filterParams * 2) :. (getRadius filterParams * 2) :.
               All) .
            A.use . A.fromList (Z :. P.length (mean filterStats)) $
            (mean filterStats)
          varArr =
            A.replicate
              (A.lift $
               Z :. (getRadius filterParams * 2) :. (getRadius filterParams * 2) :.
               All) .
            A.use . A.fromList (Z :. P.length (var filterStats)) $
            (var filterStats)
      in imagePathSource (inputFile params) $$ grayImageConduit =$= grayImage2DoubleArrayConduit =$=
         magnitudeConduitDouble
           parallelParams
           ctx
           filters
           (downsampleFactor params)
           meanArr
           varArr =$=
         buildTreeConduit parallelParams =$=
         libSVMPredictConduit parallelParams trees (radius params) (sampleRate params) =$=
         mergeSource (labelSource $ labelFile params) =$
         oneVsRestPredict
           (Parser.modelName params)
           ((Parser.modelName params) P.++ ".out")
           (labelMin, labelMax)
  destoryGPUCtx ctx
