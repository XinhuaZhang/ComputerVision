import           Application.OriginPrediction.Conduit
import           Application.RotatedMNIST.ArgsParser  as AP
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
      centerLength = 4
      numFreq = 15
      filterParams =
        FourierMellinTransformParamsGrid
        { getFourierMellinTransformGridRows = imageSize params
        , getFourierMellinTransformGridCols = imageSize params
        , getFourierMellinTransformGridRadialFreq = [0 .. fromIntegral numFreq]
        , getFourierMellinTransformGridAngularFreq = [0 .. numFreq]
        }
  writeFile (originPredictorParamsFileName params) . show $ filterParams
  (plan, filters) <-
    makeFilterConvolution getEmptyPlan filterParams Normal :: IO (DFTPlan, FourierMellinTransformConvolution)
  exportFFTWWisdom (fftwWisdomPath params)
  (x:_) <-
    runResourceT $
    CB.sourceFile (inputFile params) $$ readLabeledImagebinaryConduit =$=
    filterConduit parallelParams plan [filters] =$=
    splitOriginsConduit parallelParams centerLength (stride params) =$=
    CL.take 1
  featurePtr <-
    runResourceT $
    CB.sourceFile (inputFile params) $$ readLabeledImagebinaryConduit =$=
    filterConduit parallelParams plan [filters] =$=
    splitOriginsConduit parallelParams centerLength (stride params) =$=
    featurePtrConduitP parallelParams =$=
    CL.consume
  let trainParams =
        TrainParams
        { trainSolver = L2R_L2LOSS_SVC_DUAL
        , trainC = (c params)
        , trainNumExamples = L.length featurePtr
        , trainFeatureIndexMax = VU.length . snd $ x
        , trainModel = (originModelName params)
        }
      (labels, features) = L.unzip featurePtr
  print trainParams
  train trainParams labels features
