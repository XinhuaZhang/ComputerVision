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
  trainFeatures <- fmap (L.map VU.fromList) $ decodeFile trainFeatureNameStr
  filterParams <-
    fmap (\x -> read x :: FourierMellinTransformParamsGrid) . readFile $ paramsFilePath
  let parallelParams =
        ParallelParams
        { numThread = 28
        , batchSize = 140
        }
      (rows, cols) = read sizeStr :: (Int, Int)
      isColor = read isColorStr :: Bool
      filters =
        [ getFilterVectors
            (V4.makeFilter
               (PolarSeparableFilter filterParams Null :: FourierMellinTransformExpansionGrid)
               (div rows 2, div cols 2))
        ]
  runResourceT $
    CB.sourceFile imageListPath $$ readLabeledImagebinaryConduit =$=
    applyV4SeparableFilterComplexLabeledArrayConduit parallelParams filters =$=
    libSVMPredictConduit parallelParams trainFeatures =$=
    oneVsRestPredict modelNameStr (modelNameStr L.++ ".out") (1, 15)
