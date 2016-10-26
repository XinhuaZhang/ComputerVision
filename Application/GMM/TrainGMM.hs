module Main where

import           Application.GMM.ArgsParser         as Parser
import           Application.GMM.GMM
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
import           Data.Conduit
import           Data.Conduit.List                  as CL
import           Data.Set                           as S
import           Data.Vector                        as V
import           Prelude                            as P
import           System.Environment

main =
  do args <- getArgs
     if P.null args
        then error "run with --help to see options."
        else return ()
     params <- parseArgs args
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
     print params
     ctx <- initializeGPUCtx (Option $ gpuId params)
     case (gpuDataType params) of
       GPUFloat ->
         let filters =
               makeFilter filterParams :: PolarSeparableFilter (Acc (A.Array DIM3 (A.Complex Float)))
         in imagePathSource (inputFile params) $$ grayImageConduit =$=
            grayImage2FloatArrayConduit =$=
            magnitudeConduitFloat parallelParams
                                  ctx
                                  filters
                                  (downsampleFactor params) =$=
            CL.map (V.fromList .
                    P.map (\(PolarSeparableFeaturePoint _ _ vec) -> vec)) =$=
            gmmSink parallelParams
                    (numGaussian params)
                    (threshold params)
                    (gmmFile params)
       GPUDouble ->
         let filters =
               makeFilter filterParams :: PolarSeparableFilter (Acc (A.Array DIM3 (A.Complex Double)))
         in imagePathSource (inputFile params) $$ grayImageConduit =$=
            grayImage2DoubleArrayConduit =$=
            magnitudeConduitDouble parallelParams
                                   ctx
                                   filters
                                   (downsampleFactor params) =$=
            CL.map (V.fromList .
                    P.map (\(PolarSeparableFeaturePoint _ _ vec) -> vec)) =$=
            gmmSink parallelParams
                    (numGaussian params)
                    (threshold params)
                    (gmmFile params)
     destoryGPUCtx ctx
