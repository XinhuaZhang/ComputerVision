--import           Application.GMM.GMM
import           Application.MultiDimensionalGMM.GMM
import           Application.SIFT.ArgsParser  as Parser
import           Control.Monad                as M
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           CV.Array.LabeledArray
import           CV.Feature.SIFT
import           CV.Filter.GaussianFilter
import           CV.Filter.GaussianFilter
import           CV.Utility.Parallel          as Par
import           Data.Array.Repa              as R
import           Data.Conduit
import           Data.Conduit.Binary          as CB
import           Data.Conduit.List            as CL
import           Data.List                    as L
import           Data.Vector.Unboxed          as VU
import           System.Directory
import           System.Environment
import           Application.GMM.PCA

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
  isColor <-
    do xs <-
         runResourceT $
         sourceFile (inputFile params) $$ readLabeledImagebinaryConduit =$=
         CL.take 1
       let (LabeledArray _ arr) = L.head xs
           (Z :. nf :. _ :. _) = extent arr
       case nf of
         3 -> return True
         1 -> return False
         _ -> error $ "Images have incorrect number of channels: " L.++ show nf
  let parallelParams =
        ParallelParams
        { Par.numThread = Parser.numThread params
        , Par.batchSize = Parser.batchSize params
        }
      siftParams =
        SIFTParams
        { scaleSIFT = L.head $ scale params
        , strideSIFT = 8
        }
      gaussianParams =
        GaussianFilterParams
        { getGaussianFilterSigma = scaleSIFT siftParams
        , getGaussianFilterSize = imageSize
        }
      filePath = gmmFile params
      numM = numGaussian params
      bound = ((0, 0.5), (1, 1))
      numFeature = numPrincipal params
      gaussianFilter = makeFilter gaussianParams
      featureConduit =
        if isFixedSize params
          then siftFixedSizeConduit parallelParams siftParams gaussianFilter
          else siftVariedSizeConduit parallelParams siftParams gaussianParams
  pcaMatrix <- readMatrix (pcaFile params)
  runResourceT $
    sourceFile (inputFile params) $$ readLabeledImagebinaryConduit =$= featureConduit =$=
    pcaConduit parallelParams pcaMatrix =$=
    gmmSink (gmmFile params) numM bound (threshold params) (numExample params)
