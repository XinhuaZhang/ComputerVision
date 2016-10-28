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
import           Data.List                          as L
import           Data.Set                           as S
import           Data.Time.LocalTime
import           Data.Vector                        as V
import           Data.Vector.Unboxed                as VU
import           Foreign.Ptr
import           Prelude                            as P
import           System.Environment

trainSink
  :: ParallelParams -> FilePath -> TrainParams -> Bool -> Sink (VU.Vector Double) IO ()
trainSink parallelParams filePath trainParams findCFlag =
  do xs <- consume
     if ((VU.length . P.head $ xs) /= (trainFeatureIndexMax trainParams))
        then error $
             "Number of feature in trainParams is not correct. (" P.++
             (show . VU.length . P.head $ xs) P.++
             " vs " P.++
             (show $ trainFeatureIndexMax trainParams) P.++
             ")"
        else return ()
     featurePtrs <-
       xs `pseq` liftIO $ MP.mapM (getFeatureVecPtr . Dense . VU.toList) xs
     label <- liftIO $ readLabelFile filePath
     if findCFlag
        then liftIO $ findParameterC trainParams label featurePtrs
        else liftIO $ train trainParams label featurePtrs -- go label []
  where go :: [Double]
           -> [[Ptr C'feature_node]]
           -> Sink (VU.Vector Double) IO ()
        go label pss =
          do xs <- CL.take (Parallel.batchSize parallelParams)
             if P.length xs > 0
                then do ps <-
                          liftIO $
                          P.mapM (getFeatureVecPtr . Dense . VU.toList) xs
                        time <- liftIO getZonedTime
                        liftIO $
                          print . localTimeOfDay . zonedTimeToLocalTime $ time
                        go label $! (ps : pss)
                else liftIO $
                     train trainParams label (P.concat . L.reverse $ pss)

main =
  do args <- getArgs
     if P.null args
        then error "run with --help to see options."
        else return ()
     params <- parseArgs args
     gmm <- decodeFile (gmmFile params) :: IO GMM
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
                          numModel gmm +
                          (2 * getFilterNum filterParams) * (numModel gmm)
                       ,trainModel = modelName params}
     print params
     ctx <- initializeGPUCtx (Option $ gpuId params)
     let featureConduit =
           case (gpuDataType params) of
             GPUFloat ->
               let filters =
                     makeFilter filterParams :: PolarSeparableFilter (Acc (A.Array DIM3 (A.Complex Float)))
               in imagePathSource (inputFile params) =$= grayImageConduit =$=
                  grayImage2FloatArrayConduit =$=
                  magnitudeConduitFloat parallelParams
                                        ctx
                                        filters
                                        (downsampleFactor params)
             GPUDouble ->
               let filters =
                     makeFilter filterParams :: PolarSeparableFilter (Acc (A.Array DIM3 (A.Complex Double)))
               in imagePathSource (inputFile params) =$= grayImageConduit =$=
                  grayImage2DoubleArrayConduit =$=
                  magnitudeConduitDouble parallelParams
                                         ctx
                                         filters
                                         (downsampleFactor params)
     featureConduit =$=
       CL.map (V.fromList .
               P.map (\(PolarSeparableFeaturePoint _ _ vec) -> vec)) =$=
       (fisherVectorConduit parallelParams gmm) $$
       trainSink parallelParams
                 (labelFile params)
                 trainParams
                 (findC params) -- fisherVectorTestSink parallelParams gmm
     destoryGPUCtx ctx
