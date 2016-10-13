module Main where

import           Application.FilterStatistics.ArgsParser as Parser
import           CV.CUDA.Context
import           CV.CUDA.DataType
import           CV.Filter                               as F
import           CV.Filter.FilterStats
import           CV.Filter.PolarSeparableFilter
import           CV.Image
import           CV.IO.ImageIO
import           CV.Utility.Parallel                     as Parallel
import           Data.Array.Accelerate                   as A
import           Data.Array.Accelerate.Data.Complex      as A
import           Data.Array.Unboxed                      as AU
import           Data.Conduit
import           Data.Conduit.List                       as CL
import           Data.Set                                as S
import           GHC.Float
import           Prelude                                 as P
import           System.Environment

grayImage2FloatArrayConduit :: Conduit GrayImage IO (AU.Array (Int, Int, Int) Float)
grayImage2FloatArrayConduit =
  awaitForever
    (\img ->
        let (nx, ny) = dimensions img
        in yield .
           listArray ((0, 0, 0), (nx - 1, ny - 1, 0)) . P.map double2Float . pixelList $
           img)
           
grayImage2DoubleArrayConduit :: Conduit GrayImage IO (AU.Array (Int, Int, Int) Double)
grayImage2DoubleArrayConduit =
  awaitForever
    (\img ->
        let (nx, ny) = dimensions img
        in yield . listArray ((0, 0, 0), (nx - 1, ny - 1, 0)) . pixelList $ img)

main = do
  args <- getArgs
  if P.null args
    then error "run with --help to see options."
    else return ()
  params <- parseArgs args
  if P.null (outputFile params) 
     then error "use -o to specify output file."
     else return ()
  let parallelParams =
        ParallelParams
        { Parallel.numThread = Parser.numThread params
        , Parallel.batchSize = Parser.batchSize params
        }
      filterParams =
        PolarSeparableFilterParams
        { getRadius = 128
        , getScale = S.fromDistinctAscList [8]
        , getRadialFreq = S.fromDistinctAscList [1 .. 8]
        , getAngularFreq = S.fromDistinctAscList [1 .. 8]
        , getName = Pinwheels
        }
  ctx <- initializeGPUCtx (Option $ gpuId params)
  case (gpuDataType params) of
    GPUFloat ->
      let filters =
            F.makeFilter filterParams :: PolarSeparableFilter (Acc (A.Array DIM3 (A.Complex Float)))
      in imagePathSource (inputFile params) $$ grayImageConduit =$= grayImage2FloatArrayConduit =$=
         sink parallelParams ctx (outputFile params) filters
    GPUDouble ->
      let filters =
            F.makeFilter filterParams :: PolarSeparableFilter (Acc (A.Array DIM3 (A.Complex Double)))
      in imagePathSource (inputFile params) $$ grayImageConduit =$= grayImage2DoubleArrayConduit =$=
         sink parallelParams ctx ("N" P.++ (show $ getFilterNum filterParams) P.++ "S" P.++ (show $ getScale filterParams) P.++ (outputFile params)) filters
