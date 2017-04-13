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
  trainFeatures <- fmap (L.map VU.fromList) $ decodeFile trainFeatureNameStr
  v4QuardTreeFilterParams <-
    fmap (\x -> read x :: V4SeparableFilterParams) . readFile $ paramsFilePath
  let parallelParams = ParallelParams {numThread = 12, batchSize = 120}
      n = read sizeStr :: Int
      filterVecsList = generateV4SeparableFilter v4QuardTreeFilterParams
      isColor = read isColorStr :: Bool
  runResourceT $
    CB.sourceFile imageListPath $$ readLabeledImagebinaryConduit =$=
    applyV4SeparableFilterComplexLabeledArrayConduit
      parallelParams
      filterVecsList =$=
    libSVMPredictConduit parallelParams trainFeatures =$=
    oneVsRestPredict modelNameStr (modelNameStr L.++ ".out") (1, 15)
