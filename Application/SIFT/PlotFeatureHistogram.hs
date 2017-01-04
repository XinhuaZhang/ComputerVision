import           Application.FilterStats.FilterStats
import           Application.SIFT.ArgsParser
import           Control.Monad                       as M
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           CV.Array.LabeledArray
import           CV.Feature.SIFT
import           CV.Filter.GaussianFilter
import           CV.Utility.Parallel                 as Par
import           Data.Conduit
import           Data.Conduit.List                   as CL
import           Data.List                           as L
import           Data.Vector.Unboxed                 as VU
import           System.Environment

plotHistSink :: Sink [VU.Vector Double] (ResourceT IO) ()
plotHistSink = do
  xs <- CL.consume
  let ys = L.map VU.fromList . L.transpose . L.map VU.toList . L.concat $ xs
  liftIO $
    M.zipWithM_
      (\i vec -> plotHist vec (0, 0.25) 10 (show i) (show i L.++ ".png"))
      [1 ..]
      ys


main = do
  args <- getArgs
  if L.null args
    then error "run with --help to see options."
    else return ()
  params <- parseArgs args
  print params
  let parallelParams =
        ParallelParams
        { Par.numThread = 4
        , Par.batchSize = 4
        }
      siftParams =
        SIFTParams
        { scaleSIFT = 2
        , strideSIFT = 8
        }
      gaussianParams =
        GaussianFilterParams
        { getGaussianFilterSigma = scaleSIFT siftParams
        , getGaussianFilterSize = (0, 0)
        }
  images <- readLabeledImageBinary (inputFile params) (numGMMExample params)
  runResourceT $
    sourceList images $$ siftVariedSizeConduit parallelParams siftParams gaussianParams =$=
    plotHistSink
