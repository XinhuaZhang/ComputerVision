import           Application.Leaf.ArgsParser                  as AP
import           Application.Leaf.Conduit
import           Application.MultiDimensionalGMM.FisherKernel
import           Application.MultiDimensionalGMM.GMM
import           Classifier.LibLinear
import           Control.Concurrent.MVar                      (newMVar)
import           Control.Monad                                as M
import           Control.Monad.Trans.Resource
import           CV.Array.LabeledArray
import           CV.Feature.SIFT
import           CV.Filter.GaussianFilter                     as Gaussian
import           CV.Statistics.PCA
import           CV.Utility.Parallel                          as Par
import           CV.Utility.Time
import           Data.Array.Repa                              as R
import           Data.Binary
import           Data.Conduit
import           Data.Conduit.Binary                          as CB
import           Data.Conduit.List                            as CL
import           Data.List                                    as L
import           Data.Vector.Unboxed                          as VU
import           System.Environment
import           System.IO


main = do
  args <- getArgs
  params <- parseArgs args
  siftParams <-
    fmap (\x -> read x :: SIFTParams) . readFile $ (paramsFileName params)
  pcaMat <- decodeFile (pcaFile params) :: IO (PCAMatrix Double)
  (gmm:_) <- readGMM (gmmFile params)
  let parallelParams =
        ParallelParams
        { Par.numThread = AP.numThread params
        , Par.batchSize = AP.batchSize params
        }
      gFilterParams =
        L.map
          (\s -> GaussianFilterParams s (imageSize params) (imageSize params)) .
        gaussianScale $
        params
  lock <- newMVar ()
  gFilters <- M.mapM (Gaussian.makeFilter lock) gFilterParams
  printCurrentTime
  runResourceT $
    CB.sourceFile (inputFile params) $$ readLabeledImagebinaryConduit =$=
    labeledArraySIFTFixedSizeConduit lock parallelParams siftParams gFilters =$=
    pcaConduit parallelParams pcaMat =$=
    fisherVectorConduit parallelParams gmm =$=
    featureConduit =$=
    predict (modelName params) ((modelName params) L.++ ".out")
