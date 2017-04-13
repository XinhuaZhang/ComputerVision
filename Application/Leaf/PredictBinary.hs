import           Application.FER2013.Conduit
import           Application.RecenterImage.Conduit
import           Classifier.LibLinear
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
import           Data.Conduit
import           Data.Conduit.Binary               as CB
import           Data.Conduit.List                 as CL
import           Data.List                         as L
import           Data.Set                          as S
import           Data.Vector.Unboxed               as VU
import           System.Environment

main = do
  (imageListPath:isColorStr:paramsFilePath:sizeStr:modelName:_) <- getArgs
  v4QuardTreeFilterParams <-
    fmap (\x -> read x :: V4SeparableFilterParams) . readFile $ paramsFilePath
  let parallelParams = ParallelParams {numThread = 12, batchSize = 120}
      n = read sizeStr :: Int
      filterVecsList = generateV4SeparableFilter v4QuardTreeFilterParams
      isColor = read isColorStr :: Bool
      gaussianFilterParams = GaussianFilterParams 32 n n
      gaussianFilter = Gaussian.makeFilter gaussianFilterParams
  runResourceT $
    CB.sourceFile imageListPath $$ readLabeledImagebinaryConduit =$=
    recenterFixedSizeConduit parallelParams gaussianFilter =$=
    applyV4SeparableFilterLabeledArrayConduit parallelParams filterVecsList =$=
    featureConduitP parallelParams =$=
    predict modelName (modelName L.++ ".out")
