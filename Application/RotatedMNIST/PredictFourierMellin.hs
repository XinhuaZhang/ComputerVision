import           Application.RotatedMNIST.Conduit
import           Application.RotatedMNIST.ArgsParser  as AP
import           Classifier.LibSVM as SVM
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
import Control.Arrow


main = do
  args <- getArgs
  params <- parseArgs args
  print params
  filterParams <-
    fmap (\x -> read x :: FourierMellinTransformParamsGrid) . readFile $
    (paramsFileName params)
  -- kmeansModels <- decodeFile (kmeansFile params)
  let parallelParams =
        ParallelParams
        { Par.numThread = AP.numThread params
        , Par.batchSize = AP.batchSize params
        }
      filters =
        makeFilterExpansion
          filterParams
          (div (imageSize params) 2)
          (div (imageSize params) 2) :: FourierMellinTransformExpansion
  -- importFFTWWisdom (fftwWisdomPath params)
  -- (plan, filters) <-
  --   makeFilterConvolution getEmptyPlan filterParams Normal :: IO (DFTPlan, FourierMellinTransformConvolution)
  runResourceT $
    CB.sourceFile (inputFile params) $$ readLabeledImagebinaryConduit =$=
    filterExpansionConduit parallelParams filters =$=
    -- filterConduit parallelParams plan [filters] =$=
    -- kmeansConduit parallelParams kmeansModels =$=
    -- featureConduit =$=
    -- CL.map (second $ rescaleVector . VU.concat . L.map VU.concat) =$=
    svmFeatureConduit parallelParams =$=
    predict (AP.modelName params) ((AP.modelName params) L.++ ".out")
