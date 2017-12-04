import           Application.RotatedMNIST.ArgsParser as AP
import           Application.RotatedMNIST.Conduit
import           Classifier.LibSVM                   as SVM
import           Control.Arrow
import           Control.Monad                       as M
import           Control.Monad.Trans.Resource
import           CV.Array.LabeledArray
import           CV.Filter.FourierMellinTransform
import           CV.Utility.Parallel                 as Par
import           Data.Array.Repa
import           Data.Binary
import           Data.Complex
import           Data.Conduit
import           Data.Conduit.Binary                 as CB
import           Data.Conduit.List                   as CL
import           Data.List                           as L
import           Data.Vector.Unboxed                 as VU
import           System.Environment


main = do
  args <- getArgs
  params <- parseArgs args
  print params
  -- filterParams <-
  --   fmap (\x -> read x :: FourierMellinTransformParamsGrid) . readFile $
  --   (paramsFileName params)
  -- kmeansModels <- decodeFile (kmeansFile params)
  let parallelParams =
        ParallelParams
        { Par.numThread = AP.numThread params
        , Par.batchSize = AP.batchSize params
        }
      numFreq = 15
      filterParams =
        FourierMellinTransformParamsGrid
        { getFourierMellinTransformGridRows = imageSize params
        , getFourierMellinTransformGridCols = imageSize params
        , getFourierMellinTransformGridRadialFreq = [0 .. fromIntegral numFreq]
        , getFourierMellinTransformGridAngularFreq = [0 .. numFreq]
        }
      filters =
        makeFilterExpansion
          filterParams
          (div (imageSize params) 2)
          (div (imageSize params) 2) :: FourierMellinTransformExpansion
  writeFile (paramsFileName params) . show $ filterParams
  -- importFFTWWisdom (fftwWisdomPath params)
  -- (plan, filters) <-
  --   makeFilterConvolution getEmptyPlan filterParams Normal :: IO (DFTPlan, FourierMellinTransformConvolution)
  (x:_) <-
    runResourceT $
    CB.sourceFile (inputFile params) $$ readLabeledImagebinaryConduit =$=
    -- filterConduit parallelParams plan [filters] =$=
    filterExpansionConduit parallelParams filters =$=
    -- CL.map (second $ rescaleVector  . VU.concat . L.map VU.concat) =$=
    -- kmeansConduit parallelParams kmeansModels =$=
    CL.take 1
  featurePtr <-
    runResourceT $
    CB.sourceFile (inputFile params) $$ readLabeledImagebinaryConduit =$=
    filterExpansionConduit parallelParams filters =$=
    -- filterConduit parallelParams plan [filters] =$=
    -- kmeansConduit parallelParams kmeansModels =$=
    -- featurePtrConduit =$=
    -- CL.map (second $ rescaleVector . VU.concat . L.map VU.concat) =$=
    svmFeatureConduit parallelParams =$=
    CL.take (numGMMExample params)
      -- trainParams =
      --   TrainParams
      --   { trainSolver = L2R_L2LOSS_SVC_DUAL
      --   , trainC = (c params)
      --   , trainNumExamples = L.length featurePtr
      --   , trainFeatureIndexMax = VU.length . snd $ x
      --   , trainModel = (modelName params)
      --   }
  let trainParams =
        TrainParams
        { svmType = C_SVC
        , kernelType = RBF
        , SVM.modelName = AP.modelName params
        , numFeature = VU.length . snd $ x
        , SVM.c = AP.c params
        , eps = 0.001
        , nu = 0.5
        }
      (labels, features) = L.unzip featurePtr
  print trainParams
  train trainParams labels features
