import           Application.FilterStats.FilterStats
import           Application.GMM.PCA
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

plotHistSink :: String -> Sink [VU.Vector Double] (ResourceT IO) ()
plotHistSink str = do
  xs <- CL.consume
  let ys = L.map VU.fromList . L.transpose . L.map VU.toList . L.concat $ xs
  liftIO $
    M.zipWithM_
      (\i vec -> plotHist vec (0, 1) 100 (show i) (str L.++ show i L.++ ".png"))
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
      numTake = numPrincipal params
      numDrop = 0
  pcaMatrix <- readMatrix (pcaFile params)
  images <- readLabeledImageBinary (inputFile params) (numExample params)
  runResourceT $
    sourceList images $$ siftVariedSizeConduit parallelParams siftParams gaussianParams =$=
    plotHistSink ""
  runResourceT $
    sourceList images $$ siftVariedSizeConduit parallelParams siftParams gaussianParams =$=
    pcaConduit parallelParams pcaMatrix (numDrop, numTake) =$=
    plotHistSink "pca_"
