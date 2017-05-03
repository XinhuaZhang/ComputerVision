import           Application.Leaf.Conduit
import           Application.RecenterImage.Conduit
import           Classifier.LibSVM
import           Control.Arrow
import           Control.Monad                     as M
import           Control.Monad.Trans.Resource
import           CV.Array.Image
import           CV.Array.LabeledArray
import           CV.Filter.GaussianFilter          as Gaussian
import           CV.IO.ImageIO
import           CV.Utility.Parallel
import           CV.V4Filter                       as V4
import           Data.Array.Repa                   as R
import           Data.Binary
import           Data.Conduit
import           Data.Conduit.Binary               as CB
import           Data.Conduit.List                 as CL
import           Data.List                         as L
import           Data.Vector.Unboxed               as VU
import           System.Environment

main = do
  (imageListPath:isColorStr:paramsFilePath:sizeStr:modelNameStr:trainFeatureNameStr:_) <-
    getArgs
  let parallelParams =
        ParallelParams
        { numThread = 28
        , batchSize = 140
        }
      m = 5
      n = 5
      filterParams =
        FourierMellinTransformParamsGrid
        { getFourierMellinTransformGridRows = rows
        , getFourierMellinTransformGridCols = cols
        , getFourierMellinTransformGridScale = [64]
        , getFourierMellinTransformGridRadialFreq = [0 .. n] L.++ [-n .. (-1)]
        , getFourierMellinTransformGridAngularFreq = [0 .. m] L.++ [-m .. (-1)]
        }
      (rows, cols) = read sizeStr :: (Int, Int)
      isColor = read isColorStr :: Bool
      filters =
        [ getFilterVectors
            (V4.makeFilter
               (PolarSeparableFilter filterParams Null :: FourierMellinTransformExpansionGrid)
               (div rows 2, div cols 2))
        ]
  writeFile paramsFilePath . show $ filterParams
  featurePtr <-
    runResourceT $
    CB.sourceFile imageListPath $$ readLabeledImagebinaryConduit =$=
    applyV4SeparableFilterComplexLabeledArrayConduit parallelParams filters =$=
    CL.consume
  let trainParams =
        TrainParams
        { svmType = C_SVC
        , kernelType = PRECOMPUTED
        , modelName = modelNameStr
        , numFeature = L.length featurePtr
        , c = 512
        , eps = 0.001
        , nu = 0.5
        }
      (labels, features) = L.unzip featurePtr
      kernel = complexKernel parallelParams features
  -- kernel <- complexKernelP features
  kernelPtr <- M.zipWithM getPreComputedKernelFeatureVecPtr [1 ..] kernel
  print trainParams
  oneVsRestTrain trainParams labels kernelPtr
  encodeFile trainFeatureNameStr . L.map VU.toList $ features
