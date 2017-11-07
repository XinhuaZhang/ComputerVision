import           Application.Leaf.ArgsParser          as AP
import           Application.OriginPrediction.Conduit
import           Classifier.LibLinear
import           Control.Monad                        as M
import           Control.Monad.Trans.Resource
import           CV.Array.LabeledArray
import           CV.Filter.FourierMellinTransform
import           CV.Utility.Parallel                  as Par
import           Data.Array.Repa
import           Data.Binary
import           Data.Complex
import           Data.Conduit
import           Data.Conduit.Binary                  as CB
import           Data.Conduit.List                    as CL
import           Data.List                            as L
import           Data.Vector.Unboxed                  as VU
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
      len = 16
      n = 15
      filterParams =
        FourierMellinTransformParamsGrid
        { getFourierMellinTransformGridRows = imageSize params
        , getFourierMellinTransformGridCols = imageSize params
        , getFourierMellinTransformGridRadialFreq = [0 .. fromIntegral n]
        , getFourierMellinTransformGridAngularFreq = [0 .. n]
        }
  writeFile (paramsFileName params) . show $ filterParams
  (plan, filters) <-
    makeFilterConvolution getEmptyPlan filterParams Normal :: IO (DFTPlan, FourierMellinTransformConvolution)
  exportFFTWWisdom (fftwWisdomPath params)
  (x:_) <-
    runResourceT $
    CB.sourceFile (inputFile params) $$ readLabeledImagebinaryConduit =$=
    filterConduit parallelParams plan [filters] =$=
    splitOriginsConduit parallelParams len (stride params) =$=
    CL.take 1
  featurePtr <-
    runResourceT $
    CB.sourceFile (inputFile params) $$ readLabeledImagebinaryConduit =$=
    filterConduit parallelParams plan [filters] =$=
    splitOriginsConduit parallelParams len (stride params) =$=
    featurePtrConduitP parallelParams =$=
    CL.consume
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
