import           Application.FER2013.Conduit
import           Application.Leaf.Conduit
import           Classifier.LibSVM
import           Control.Arrow
import           Control.Monad                as M
import           Control.Monad.Trans.Resource
import           CV.Array.Image
import           CV.Array.LabeledArray
import           CV.IO.ImageIO
import           CV.Utility.Parallel
import           CV.V4Filter
import           Data.Array.Repa              as R
import           Data.Conduit
import           Data.Conduit.Binary          as CB
import           Data.Conduit.List            as CL
import           Data.List                    as L
import           Data.Vector.Unboxed          as VU
import           System.Environment
import Data.Binary

main = do
  (imageListPath:isColorStr:paramsFilePath:sizeStr:modelNameStr:trainFeatureNameStr:_) <-
    getArgs
  let parallelParams = ParallelParams {numThread = 12, batchSize = 120}
      v4QuardTreeFilterParams =
        V4SeparableFilterParams
        { separableFilterRows = n
        , separableFilterCols = n
        , polarSeparableScale = [2 ** (i / 2) | i <- [6 .. 9]]
        , polarSeparableRadialFreq =  L.take 8 [0,4 ..]
        , polarSeparableAngularFreq = [0 .. 7]
        , cartesianGratingScale = [2 ** (i / 2) | i <- [7 .. 10]]
        , cartesianGratingFreq = [1 .. 8]
        , cartesianGratingAngle = [0,15 .. 360 - 15]
        , hyperbolicSeparableScale = [2 ** (i / 2) | i <- [7 .. 10]]
        , hyperbolicSeparableUFreq = [0 .. 3]
        , hyperbolicSeparableVFreq = [0 .. 7]
        , hyperbolicSeparableAngle = 90
        , separableFilterParams = P
        }
      n = read sizeStr :: Int
      filterVecsList = generateV4SeparableFilter v4QuardTreeFilterParams
      isColor = read isColorStr :: Bool
  writeFile paramsFilePath . show $ v4QuardTreeFilterParams
  featurePtr <-
    runResourceT $
    CB.sourceFile imageListPath $$ readLabeledImagebinaryConduit =$=
    applyV4SeparableFilterComplexLabeledArrayConduit
      parallelParams
      filterVecsList =$=
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
