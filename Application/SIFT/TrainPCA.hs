import           Application.GMM.PCA
import           Application.SIFT.ArgsParser
import           Control.Monad                as M
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           CV.Array.LabeledArray
import           CV.Feature.SIFT
import           CV.Filter.GaussianFilter
import           CV.Utility.Parallel          as Par
import           Data.Conduit
import           Data.Conduit.Binary          as CB
import           Data.Conduit.List            as CL
import           Data.List                    as L
import           Data.Vector.Unboxed          as VU
import           System.Environment

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
        { scaleSIFT = L.head $ scale params
        , strideSIFT = 8
        }
      gaussianParams =
        GaussianFilterParams
        { getGaussianFilterSigma = scaleSIFT siftParams
        , getGaussianFilterSize = (0, 0)
        }
  pcaMatrix <-
    runResourceT $
    sourceFile (inputFile params) $$ readLabeledImagebinaryConduit =$=
    siftVariedSizeConduit parallelParams siftParams gaussianParams =$=
    pcaSink (numExample params) (numPrincipal params)
  writeMatrix (pcaFile params) pcaMatrix
