import           Application.GMM.PCA
import           Application.MultiDimensionalGMM.FisherKernel
import           Application.MultiDimensionalGMM.GMM
import           Application.MultiDimensionalGMM.MixtureModel
import           Application.SIFT.ArgsParser                  as Parser
import           Classifier.LibLinear
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
import           Foreign.Ptr
import           System.Directory
import           System.Environment

trainSink
  :: ParallelParams
  -> FilePath
  -> TrainParams
  -> Bool
  -> Sink (Int, VU.Vector Double) (ResourceT IO) ()
trainSink parallelParams filePath trainParams findCFlag = go [] []
  where
    go :: [[Double]]
       -> [[Ptr C'feature_node]]
       -> Sink (Int, VU.Vector Double) (ResourceT IO) ()
    go label pss = do
      xs <- CL.take (Par.batchSize parallelParams)
      if L.length xs > 0
        then do
          let (ls, ys) = L.unzip xs
          ps <- liftIO $ M.mapM (getFeatureVecPtr . Dense . VU.toList) ys
          liftIO $ printCurrentTime
          go ((L.map fromIntegral ls) : label) $! (ps : pss)
        else liftIO $
             train
               trainParams
               (L.concat . L.reverse $ label)
               (L.concat . L.reverse $ pss)

main = do
  args <- getArgs
  if L.null args
    then error "run with --help to see options."
    else return ()
  params <- parseArgs args
  print params
  gmm <- decodeFile (gmmFile params)
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
  imageListLen <- getArrayNumFile (inputFile params)
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
      trainParams =
        TrainParams
        { trainSolver = L2R_L2LOSS_SVC_DUAL
        , trainC = c params
        , trainNumExamples = imageListLen
        , trainFeatureIndexMax = (2 * numFeature) * (numModel gmm)
        , trainModel = modelName params
        }
      numFeature = numPrincipal params
      gaussianFilter = makeFilter gaussianParams
      featureConduit =
        if isFixedSize params
          then labeledArraySIFTFixedSizeConduit parallelParams siftParams gaussianFilter
          else labeledArraySIFTVariedSizeConduit parallelParams siftParams gaussianParams
  print trainParams
  runResourceT $
    sourceFile (inputFile params) $$ readLabeledImagebinaryConduit =$= featureConduit =$=
    pcaLabelConduit parallelParams pcaMatrix =$=
    (fisherVectorConduit parallelParams gmm) =$=
    trainSink parallelParams (labelFile params) trainParams (findC params)
