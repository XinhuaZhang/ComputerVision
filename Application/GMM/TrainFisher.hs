module Main where

import           Application.GMM.ArgsParser         as Parser
import           Application.GMM.FisherKernel
import           Application.GMM.GMM
import           Application.GMM.MixtureModel
import           Classifier.LibLinear
import           Control.Monad.IO.Class
import qualified Control.Monad.Parallel             as MP
import           Control.Parallel
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
import           Data.Set                           as S
import           Data.Vector                        as V
import           Data.Vector.Unboxed                as VU
import           Prelude                            as P
import           System.Environment



trainSink
  :: FilePath -> TrainParams -> Sink (VU.Vector Double) IO ()
trainSink filePath trainParams =
  do xs <- consume
     featurePtrs <- xs `pseq` liftIO $ MP.mapM (getFeatureVecPtr . Dense . VU.toList) xs
     label <- liftIO $ readLabelFile filePath
     liftIO $ train trainParams label featurePtrs

main =
  do args <- getArgs
     if P.null args
        then error "run with --help to see options."
        else return ()
     params <- parseArgs args
     gmm <- decodeFile (gmmFile params)
     imageList <- readFile (inputFile params)
     let parallelParams =
           ParallelParams {Parallel.numThread = Parser.numThread params
                          ,Parallel.batchSize = Parser.batchSize params}
         filterParams =
           PolarSeparableFilterParams {getRadius = 128
                                      ,getScale = S.fromDistinctAscList [8]
                                      ,getRadialFreq =
                                         S.fromDistinctAscList [0 .. 3]
                                      ,getAngularFreq =
                                         S.fromDistinctAscList [0 .. 3]
                                      ,getName = Pinwheels}
         trainParams =
           TrainParams {trainSolver = L2R_L2LOSS_SVC_DUAL
                       ,trainC = c params
                       ,trainNumExamples = P.length . lines $ imageList
                       ,trainFeatureIndexMax =
                          (numModel gmm - 1) +
                          (getFilterNum filterParams) * (numModel gmm)
                       ,trainModel = modelName params}
     print params
     ctx <- initializeGPUCtx (Option $ gpuId params)
     case (gpuDataType params) of
       GPUFloat ->
         let filters =
               makeFilter filterParams :: PolarSeparableFilter (Acc (A.Array DIM3 (A.Complex Float)))
         in do imagePathSource (inputFile params) $$ grayImageConduit =$=
                 grayImage2FloatArrayConduit =$=
                 magnitudeConduitFloat parallelParams
                                       ctx
                                       filters
                                       (downsampleFactor params) =$=
                 CL.map (V.fromList .
                         P.map (\(PolarSeparableFeaturePoint _ _ vec) -> vec)) =$=
                 -- fisherVectorTestSink parallelParams gmm
                 fisherVectorConduit parallelParams gmm =$=
                 trainSink (labelFile params) trainParams
       -- imagePathSource (inputFile params) $$ grayImageConduit =$=
       --   grayImage2FloatArrayConduit =$=
       --   magnitudeConduitFloat parallelParams
       --                         ctx
       --                         filters
       --                         (downsampleFactor params) =$=
       --   CL.map (V.fromList .
       --           P.map (\(PolarSeparableFeaturePoint _ _ vec) -> vec)) =$=
       --   fisherVectorSink parallelParams gmm "hehe.dat"
       GPUDouble ->
         let filters =
               makeFilter filterParams :: PolarSeparableFilter (Acc (A.Array DIM3 (A.Complex Double)))
         in do imagePathSource (inputFile params) $$ grayImageConduit =$=
                 grayImage2DoubleArrayConduit =$=
                 magnitudeConduitDouble parallelParams
                                       ctx
                                       filters
                                       (downsampleFactor params) =$=
                 CL.map (V.fromList .
                         P.map (\(PolarSeparableFeaturePoint _ _ vec) -> vec)) =$=
                 fisherVectorConduit parallelParams gmm =$=
                 trainSink (labelFile params) trainParams
     destoryGPUCtx ctx
