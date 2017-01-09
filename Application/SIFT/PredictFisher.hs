import           Application.GMM.PCA
import           Application.MultiDimensionalGMM.FisherKernel
import           Application.MultiDimensionalGMM.GMM
import           Application.MultiDimensionalGMM.MixtureModel
import           Application.SIFT.ArgsParser                  as Parser
import           Classifier.LibLinear
import           Control.Arrow
import           Control.Monad                                as M
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           CV.Array.LabeledArray
import           CV.Feature.SIFT
import           CV.Filter.GaussianFilter
import           CV.Filter.GaussianFilter
import           CV.Utility.Parallel                          as Par
import           CV.Utility.Time
import           Data.Array.Repa                              as R
import           Data.Binary
import           Data.Conduit
import           Data.Conduit.Binary                          as CB
import           Data.Conduit.List                            as CL
import           Data.List                                    as L
import           Data.Vector.Unboxed                          as VU
import           System.Directory
import           System.Environment

main = do
  args <- getArgs
  if L.null args
    then error "run with --help to see options."
    else return ()
  params <- parseArgs args
  print params
  gmm <- decodeFile (gmmFile params) :: IO GMM
  pcaMatrix <- readMatrix (pcaFile params)
  imageListLen <- getArrayNumFile (inputFile params)
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
  imageListLen <- getArrayNumFile (inputFile params)
  let parallelParams =
        ParallelParams
        { Par.numThread = Parser.numThread params
        , Par.batchSize = Parser.batchSize params
        }
      siftParams =
        SIFTParams
        { scaleSIFT = L.head $ scale params
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
          then labeledArraySIFTFixedSizeConduit parallelParams siftParams gaussianFilter
          else labeledArraySIFTVariedSizeConduit parallelParams siftParams gaussianParams
  runResourceT $
    sourceFile (inputFile params) $$ readLabeledImagebinaryConduit =$= featureConduit =$=
    pcaLabelConduit parallelParams pcaMatrix =$=
    (fisherVectorConduit parallelParams gmm) =$=
    CL.map (fromIntegral *** (getFeature . Dense . VU.toList)) =$=
    predict (modelName params) ((modelName params) L.++ ".out")
