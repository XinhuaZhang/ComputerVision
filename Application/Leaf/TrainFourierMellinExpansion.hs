import           Application.Leaf.ArgsParser      as AP
import           Application.Leaf.Conduit
import           Classifier.LibLinear
import           Control.Monad                    as M
import           Control.Monad.Trans.Resource
import           CV.Array.LabeledArray
import           CV.Filter.FourierMellinTransform
import           CV.Statistics.KMeans
import           CV.Utility.FFT
import           CV.Utility.Parallel              as Par
import           Data.Array.Repa
import           Data.Binary
import           Data.Complex
import           Data.Conduit
import           Data.Conduit.Binary              as CB
import           Data.Conduit.List                as CL
import           Data.List                        as L
import           Data.Vector.Unboxed              as VU
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
      filterParams =
        FourierMellinTransformParamsGrid
        { getFourierMellinTransformGridRows = imageSize params
        , getFourierMellinTransformGridCols = imageSize params
        , getFourierMellinTransformGridRadialFreq = [-15 .. 15]
        , getFourierMellinTransformGridAngularFreq = [-15 .. 15]
        }
      filter =
        makeFilterExpansion
          filterParams
          (div (imageSize params) 2)
          (div (imageSize params) 2) :: FourierMellinTransformExpansion
  writeFile (paramsFileName params) . show $ filterParams
  (x:_) <-
    runResourceT $
    CB.sourceFile (inputFile params) $$ readLabeledImagebinaryConduit =$=
    filterExpansionConduit parallelParams filter =$=
    CL.take 1
  featurePtr <-
    runResourceT $
    CB.sourceFile (inputFile params) $$ readLabeledImagebinaryConduit =$=
    filterExpansionConduit parallelParams filter =$=
    featurePtrConduit =$=
    CL.consume
  let trainParams =
        TrainParams
        { trainSolver = L2R_L2LOSS_SVC_DUAL
        , trainC = (c params)
        , trainNumExamples = L.length featurePtr
        , trainFeatureIndexMax = fromIntegral . VU.length . snd $ x
        , trainModel = (modelName params)
        }
      (labels, features) = L.unzip featurePtr
  print trainParams
  train trainParams labels features
