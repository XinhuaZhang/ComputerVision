module Main where

import           Application.KDTree.ArgsParser      as Parser
import           Application.KDTree.Conduit
import           Application.KDTree.KDTree
import           Classifier.LibSVM                  as SVM
import           CV.CUDA.Context
import           CV.CUDA.DataType
import           CV.Feature.PolarSeparable
import           CV.Filter
import           CV.Filter.PolarSeparableFilter
import           CV.IO.ImageIO
import           CV.Utility.Parallel                as Parallel
import           Data.Array.Accelerate              as A
import           Data.Array.Accelerate.Data.Complex as A
import           Data.Conduit
import           Data.Conduit.List                  as CL
import           Data.KdTree.Static                 as KDT
import qualified Data.Set                           as S
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
      trainParams =
        TrainParams
        { svmType = C_SVC
        , kernelType = PRECOMPUTED
        , SVM.modelName = Parser.modelName params
        , numFeature = getFilterNum filterParams
        , SVM.c = Parser.c params
        , eps = 0.001
        , nu = 0.5
        }
  ctx <- initializeGPUCtx (Option $ gpuId params)
  print params
  trees <- case (gpuDataType params) of
             GPUFloat ->
               let filters =
                     makeFilter filterParams :: PolarSeparableFilter (Acc (A.Array DIM3 (A.Complex Float)))
               in imagePathSource (inputFile params) $$ grayImageConduit =$= grayImage2FloatArrayConduit =$=
                  magnitudeConduitFloat parallelParams ctx filters =$=
                  buildTreeConduit parallelParams =$=
                  libSVMTrainSink (labelFile params) parallelParams trainParams (radius params)
             GPUDouble ->
               let filters =
                     makeFilter filterParams :: PolarSeparableFilter (Acc (A.Array DIM3 (A.Complex Double)))
               in imagePathSource (inputFile params) $$ grayImageConduit =$= grayImage2DoubleArrayConduit =$=
                  magnitudeConduitDouble parallelParams ctx filters =$=
                  buildTreeConduit parallelParams =$=
                  libSVMTrainSink (labelFile params) parallelParams trainParams (radius params)
  writeFile "featurePoints.dat" (show . P.map KDT.toList $ trees)
  destoryGPUCtx ctx
