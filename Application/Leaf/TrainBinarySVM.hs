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
import           CV.V4Filter
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
        { numThread = 12
        , batchSize = 120
        }
      m = 7
      v4QuardTreeFilterParams =
        V4SeparableFilterParamsAxis
        { separableFilterRows = rows
        , separableFilterCols = cols
        , polarSeparableScale = [64]
        , polarSeparableFreq = [1 .. 8]
        , polarSeparableRadialMultiplier = [-m,-(m - 1) .. m]
        , polarSeparableAngularMultiplier = [-m,-(m - 1) .. m]
        , cartesianGratingScale =
          [ 2 ** (i / 2)
          | i <- [7 .. 10] ]
        , cartesianGratingFreq = L.take 8 [1 .. 8]
        , cartesianGratingAngle = [0,15 .. 360 - 15]
        , hyperbolicSeparableScale =
          [ 2 ** (i / 2)
          | i <- [7 .. 10] ]
        , hyperbolicSeparableUFreq = [0 .. 3]
        , hyperbolicSeparableVFreq = [0 .. 3]
        , hyperbolicSeparableAngle = 15
        , separableFilterParams = P
        }
      (rows, cols) = read sizeStr :: (Int, Int)
      isColor = read isColorStr :: Bool
      gaussianFilterParams = GaussianFilterParams 32 rows cols
      gaussianFilter = Gaussian.makeFilter gaussianFilterParams
  writeFile paramsFilePath . show $ v4QuardTreeFilterParams
  featurePtr <-
    runResourceT $
    CB.sourceFile imageListPath $$ readLabeledImagebinaryConduit =$=
    applyV4SeparableFilterComplexLabeledArrayWithCenterConduit
      parallelParams
      gaussianFilter
      v4QuardTreeFilterParams =$=
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
