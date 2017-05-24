import           Application.Leaf.ArgsParser                  as AP
import           Application.Leaf.Conduit
import           Application.MultiDimensionalGMM.FisherKernel
import           Application.MultiDimensionalGMM.GMM
import           Classifier.LibLinear
import           Control.Monad                                as M
import           Control.Monad.Trans.Resource
import           CV.Array.LabeledArray
import           CV.Statistics.PCA
import           CV.Utility.Parallel                          as Par
import           CV.V4Filter
import           CV.V4FilterConvolution
import           Data.Binary
import           Data.Conduit
import           Data.Conduit.Binary                          as CB
import           Data.Conduit.List                            as CL
import           Data.List                                    as L
import           Data.Vector.Unboxed                          as VU
import           System.Environment
import           System.IO
import           Control.Concurrent.MVar             (newMVar)
import           CV.Filter.GaussianFilter            as Gaussian
import Data.Array.Repa as R

main = do
  args <- getArgs
  params <- parseArgs args
  filterParams <-
    fmap (\x -> read x :: V4SeparableFilterParamsAxis) . readFile $
    (paramsFileName params)
  pcaMat <- decodeFile (pcaFile params) :: IO (PCAMatrix Double)
  (gmm:_) <- readGMM (gmmFile params)
  let parallelParams =
        ParallelParams
        { Par.numThread = AP.numThread params
        , Par.batchSize = AP.batchSize params
        }
      filters = generateV4SeparableFilterAxis filterParams
      gFilterParams =
        L.map
          (\s -> GaussianFilterParams s (imageSize params) (imageSize params)) .
        gaussianScale $
        params
  lock <- newMVar ()
  filtersF <-
    M.mapM (fourierTransformFilter lock (imageSize params, imageSize params)) filters
  gFilters <-
    M.mapM (fmap (getFilter . fmap toUnboxed) . Gaussian.makeFilter lock) gFilterParams
  runResourceT $
    CB.sourceFile (inputFile params) $$ readLabeledImagebinaryConduit =$=
    applyV4SeparableFilterConvolutionLabeledArrayConduit lock parallelParams 1 gFilters filtersF  =$=
    orientationHistogramConduit
      parallelParams
      (patchSize params)
      (stride params)
      (numBin params) =$=
    pcaConduit parallelParams pcaMat =$=
    fisherVectorConduit parallelParams gmm =$=
    featureConduit =$=
    predict (modelName params) ((modelName params) L.++ ".out")
