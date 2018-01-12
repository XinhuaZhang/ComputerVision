import           Application.ECCV2018.ArgsParser          as AP
import           Application.ECCV2018.Conduit
import           Application.ECCV2018.Convolution.Conduit
import           Classifier.LibLinear
import           Control.Monad                            as M
import           Control.Monad.Trans.Resource
import           CV.Array.LabeledArray
import           CV.Filter.FourierMellinTransform
import           CV.Filter.PolarSeparableFilter
import           CV.Utility.Parallel                      as Par
import           Data.Array.Repa
import           Data.Binary
import           Data.Conduit
import           Data.Conduit.Binary                      as CB
import           Data.Conduit.List                        as CL
import           Data.List                                as L
import           Data.Vector.Unboxed                      as VU
import           System.Environment

main = do
  args <- getArgs
  params <- parseArgs args
  print params
  let parallelParams =
        ParallelParams
        { Par.numThread = AP.numThread params
        , Par.batchSize = AP.batchSize params
        }
  kmeansModel <- decodeFile (kmeansFile params)
  filterParams <-
    fmap (\x -> read x :: PolarSeparableFilterParams) . readFile $
    (paramsFileName params)
  (plan, filters) <-
    makePolarSeparableFilterConvolution getEmptyPlan filterParams
  (x:_) <-
    runResourceT $
    CB.sourceFile (inputFile params) $$ readLabeledImagebinaryConduit =$=
    filterConvolutionConduit parallelParams plan filters =$=
    invariantFeatureExtractionConduit parallelParams (stride params) =$=
    kmeansConduit parallelParams kmeansModel =$=
    CL.take 1
  featurePtr <-
    runResourceT $
    CB.sourceFile (inputFile params) $$ readLabeledImagebinaryConduit =$=
    filterConvolutionConduit parallelParams plan filters =$=
    invariantFeatureExtractionConduit parallelParams (stride params) =$=
    kmeansConduit parallelParams kmeansModel =$=
    featurePtrConduit =$=
    CL.take (numGMMExample params)
  let trainParams =
        TrainParams
        { trainSolver = L2R_L2LOSS_SVC_DUAL
        , trainC = (c params)
        , trainNumExamples = L.length featurePtr
        , trainFeatureIndexMax = VU.length . snd $ x
        , trainModel = (modelName params)
        }
      (labels, features) = L.unzip featurePtr
  print trainParams
  train trainParams labels features
