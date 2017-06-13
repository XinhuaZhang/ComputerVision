import           Application.Leaf.ArgsParser         as AP
import           Application.Leaf.Conduit
import           Application.MultiDimensionalGMM.GMM
import           Control.Concurrent.MVar             (newMVar)
import           Control.Monad                       as M
import           Control.Monad.Trans.Resource
import           CV.Array.LabeledArray
import           CV.Feature.SIFT
import           CV.Filter.GaussianFilter            as Gaussian
import           CV.Utility.Parallel                 as Par
import           Data.Array.Repa                     as R
import           Data.Conduit
import           Data.Conduit.Binary                 as CB
import           Data.Conduit.List                   as CL
import           Data.List                           as L
import           Data.Vector.Unboxed                 as VU
import           System.Environment
import           System.IO

main = do
  args <- getArgs
  params <- parseArgs args
  print params
  let parallelParams =
        ParallelParams
        { Par.numThread = AP.numThread params
        , Par.batchSize = AP.batchSize params
        }
      siftParams =
        SIFTParams
        { scaleSIFT = [1]
        , strideSIFT = stride params
        }
      gFilterParams =
        L.map
          (\s -> GaussianFilterParams s (imageSize params) (imageSize params)) .
        gaussianScale $
        params
  lock <- newMVar ()
  gFilters <- M.mapM (Gaussian.makeFilter lock) gFilterParams
  writeFile (paramsFileName params) . show $ siftParams
  (_, vecs) <-
    runResourceT $
    CB.sourceFile (inputFile params) $$ readLabeledImagebinaryConduit =$=
    labeledArraySIFTFixedSizeConduit lock parallelParams siftParams gFilters =$=
    pcaSink
      parallelParams
      (pcaFile params)
      (numPrincipal params)
      (numGMMExample params)
  withBinaryFile (gmmFile params) WriteMode $
    \h ->
       runResourceT $
       CL.sourceList vecs $$
       hGMMSink1
         parallelParams
         h
         (numGaussian params)
         (threshold params)
         (numGMMExample params)
