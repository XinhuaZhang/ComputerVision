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
  trainFeatures <- fmap (L.map VU.fromList) $ decodeFile trainFeatureNameStr
  v4QuardTreeFilterParams <-
    fmap (\x -> read x :: V4SeparableFilterParamsAxis) . readFile $ paramsFilePath
  let parallelParams =
        ParallelParams
        { numThread = 12
        , batchSize = 120
        }
      (rows, cols) = read sizeStr :: (Int, Int)
      isColor = read isColorStr :: Bool
      gaussianFilterParams = GaussianFilterParams 32 rows cols
      gaussianFilter = Gaussian.makeFilter gaussianFilterParams
  runResourceT $
    CB.sourceFile imageListPath $$ readLabeledImagebinaryConduit =$=
    applyV4SeparableFilterComplexLabeledArrayWithCenterConduit
      parallelParams
      gaussianFilter
      v4QuardTreeFilterParams =$=
    libSVMPredictConduit parallelParams trainFeatures =$=
    oneVsRestPredict modelNameStr (modelNameStr L.++ ".out") (1, 15)
