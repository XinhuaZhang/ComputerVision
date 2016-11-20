module Main where

import           Application.GMM.ArgsParser         as Parser
import           Application.GMM.MPPCA
import           Application.GMM.PPCA
import           CV.CUDA.Context
import           CV.CUDA.DataType
import           CV.Feature.PolarSeparable
import           CV.Feature.PolarSeparableAcc
import           CV.Filter
import           CV.Filter.FilterStats
import           CV.Filter.PolarSeparableFilter
import           CV.Filter.PolarSeparableFilterAcc
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
import Data.Matrix

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
                                      ,getScale =
                                         S.fromDistinctAscList (scale params)
                                      ,getRadialFreq =
                                         S.fromDistinctAscList
                                           [0 .. (freq params - 1)]
                                      ,getAngularFreq =
                                         S.fromDistinctAscList
                                           [0 .. (freq params - 1)]
                                      ,getName = Pinwheels}
         ppcaInitParams =
           PPCAInitParams {numPrincipal = 16 
                          ,wRange = (0,1)
                          ,muRange = (0,10)
                          ,sigmaRange = (1,100)}
     print params
     ctx <- initializeGPUCtx (Option $ gpuId params)
     let featureConduit =
           case (gpuDataType params) of
             GPUFloat ->
               let filters =
                     makeFilter filterParams :: PolarSeparableFilter (Acc (A.Array DIM3 (A.Complex Float)))
               in imagePathSource (inputFile params) =$= grayImageConduit =$=
                  grayImage2FloatArrayConduit =$=
                  if isComplex params
                     then complexConduitFloat parallelParams
                                              ctx
                                              filters
                                              (downsampleFactor params)
                     else magnitudeConduitFloat parallelParams
                                                ctx
                                                filters
                                                (downsampleFactor params)
             GPUDouble ->
               let filters =
                     makeFilter filterParams :: PolarSeparableFilter (Acc (A.Array DIM3 (A.Complex Double)))
               in imagePathSource (inputFile params) =$= grayImageConduit =$=
                  grayImage2DoubleArrayConduit =$=
                  if isComplex params
                     then complexConduitDouble parallelParams
                                               ctx
                                               filters
                                               (downsampleFactor params)
                     else magnitudeConduitDouble parallelParams
                                                 ctx
                                                 filters
                                                 (downsampleFactor params)
     featureConduit $$
       CL.map (V.fromList .
               P.map (\(PolarSeparableFeaturePoint _ _ vec) ->
                        colVector . V.convert $ vec)) =$=
       mppcaSink parallelParams
                 ppcaInitParams
                 (numGaussian params)
                 (threshold params)
                 (gmmFile params)
     destoryGPUCtx ctx
