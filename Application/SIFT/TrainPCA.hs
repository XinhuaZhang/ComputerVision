import           Application.GMM.PCA
import           Application.SIFT.ArgsParser
import           Control.Monad                as M
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           CV.Array.LabeledArray
import           CV.Feature.SIFT
import           CV.Filter.GaussianFilter
import           CV.Utility.Parallel          as Par
import           Data.Array.Repa              as R
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
  imageSize <-
    if isFixedSize params
      then do
        xs <-
          runResourceT $
          sourceFile (inputFile params) $$ readLabeledImagebinaryConduit =$=
          CL.take 1
        let (LabeledArray _ arr) = L.head xs
            (Z :. _ :. ny :. nx) = extent arr
        return (ny, nx)
      else return (0, 0)
  let parallelParams =
        ParallelParams
        { Par.numThread = 4
        , Par.batchSize = 4
        }
      siftParams =
        SIFTParams
        { scaleSIFT = scale params
        , strideSIFT = downsampleFactor params
        }
      gaussianParams =
        L.map
          (\s ->
              GaussianFilterParams
              { getGaussianFilterSigma = s
              , getGaussianFilterSize = imageSize
              }) .
        scaleSIFT $
        siftParams
      gaussianFilter = L.map makeFilter gaussianParams
      featureConduit =
        if isFixedSize params
          then siftFixedSizeConduit parallelParams siftParams gaussianFilter
          else siftVariedSizeConduit parallelParams siftParams gaussianParams
  pcaMatrix <-
    runResourceT $
    sourceFile (inputFile params) $$ readLabeledImagebinaryConduit =$= featureConduit =$=
    pcaSink (numExample params) (numPrincipal params)
  writeMatrix (pcaFile params) pcaMatrix
