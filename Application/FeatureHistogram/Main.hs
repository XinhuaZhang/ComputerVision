module Main where

import           Control.Monad                      as M
import           Control.Monad.IO.Class             (liftIO)
import           CV.CUDA.Context
import           CV.Feature.PolarSeparable
import           CV.Filter                          as F
import           CV.Filter.PolarSeparableFilter
import           CV.Image
import           CV.IO.ImageIO
import           CV.Utility.Parallel
import           Data.Array.Accelerate              as A
import           Data.Array.Accelerate.CUDA         as A
import           Data.Array.Accelerate.Data.Complex as A
import           Data.Array.Unboxed                 as AU
import           Data.Conduit
import           Data.Conduit.List                  as CL
import qualified Data.Set                           as S
import           Data.Vector.Unboxed                as VU
import           GHC.Float
import           Graphics.Gnuplot.Simple
import           Prelude                            as P

featureHistogramSink :: Sink [PolarSeparableFeaturePoint] IO ()
featureHistogramSink = do
  xs <- consume
  let ys = P.concatMap (P.map (P.last . feature)) xs :: [Double]
      zs = P.map (\y -> ((P.floor (y / 0.01) + 100), 1 :: Int)) ys
      hist =
        VU.accum (+) (VU.replicate 200 0) $
        P.filter ((<200) . P.fst) $
        P.filter ((>(-1)) . P.fst) $
        withStrategy (parListChunk (div (P.length zs) 8) rdeepseq) zs
  liftIO $
    plotListStyle
      [PNG "hist.png"]
      (PlotStyle LinesPoints (CustomStyle [LineWidth 1])) $
    VU.toList hist

grayImage2FloatArrayConduit :: Conduit GrayImage IO (AU.Array (Int, Int, Int) Float)
grayImage2FloatArrayConduit =
  awaitForever
    (\img ->
        let (nx, ny) = dimensions img
        in yield .
           AU.listArray ((0, 0, 0), (nx - 1, ny - 1, 0)) . P.map double2Float . pixelList $
           img)

main = do
  let parallelParams =
        ParallelParams
        { numThread = 2
        , batchSize = 10
        }
      filterParams =
        PolarSeparableFilterParams
        { getRadius = 128
        , getScale = S.fromDistinctAscList [8]
        , getRadialFreq = S.fromDistinctAscList [0 .. 3]
        , getAngularFreq = S.fromDistinctAscList [0 .. 3]
        , getName = Pinwheels
        }
      filters =
        F.makeFilter filterParams :: PolarSeparableFilter (Acc (A.Array DIM3 (A.Complex Float)))
      imagePath = "/home/xinhua/Workspace/Polar-Separable-Filter/Dataset/Caltech101/Test/Original/Gray/imageList_100.txt"
  ctx <- initializeGPUCtx (Option [0])
  imagePathSource imagePath $$ grayImageConduit =$= grayImage2FloatArrayConduit =$=
    magnitudeConduitFloat' parallelParams ctx filters 2 =$=
    featureHistogramSink
  destoryGPUCtx ctx
