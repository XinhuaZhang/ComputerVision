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
import           Data.Vector.Unboxed               as VU
import           System.Environment

main = do
  (imageListPath:isColorStr:paramsFilePath:sizeStr:modelName:_) <- getArgs
  let parallelParams = ParallelParams {numThread = 12, batchSize = 120}
      deg = 15
      v4QuardTreeFilterParams =
        V4SeparableFilterParams
        { separableFilterRows = n * 2
        , separableFilterCols = n * 2
        , polarSeparableScale = [2 ** (i / 2) | i <- [5 .. 8]]
        , polarSeparableFreq = [1..8]
        , polarSeparableAngle = [0,deg..90-deg]
        , cartesianGratingScale = [2 ** (i / 2) | i <- [7 .. 10]]
        , cartesianGratingFreq = L.take 8 [1 .. 8]
        , cartesianGratingAngle = [0,15 .. 360 - 15]
        , hyperbolicSeparableScale = [2 ** (i / 2) | i <- [7 .. 10]]
        , hyperbolicSeparableUFreq = [0 .. 3]
        , hyperbolicSeparableVFreq = [0 .. 7]
        , hyperbolicSeparableAngle = 15
        , separableFilterParams = P
        }
      n = read sizeStr :: Int
      filterVecsList = generateV4SeparableFilter v4QuardTreeFilterParams
      isColor = read isColorStr :: Bool
      gaussianFilterParams = GaussianFilterParams 32 n n
      gaussianFilter = Gaussian.makeFilter gaussianFilterParams
  writeFile paramsFilePath . show $ v4QuardTreeFilterParams
  featurePtr <-
    runResourceT $
    CB.sourceFile imageListPath $$ readLabeledImagebinaryConduit =$=
    recenterFixedSizeConduit parallelParams gaussianFilter =$=
    applyV4SeparableFilterLabeledArrayConduit parallelParams filterVecsList =$=
    featurePtrConduit =$=
    CL.consume
  featurePtr1 <-
    runResourceT $
    CB.sourceFile imageListPath $$ readLabeledImagebinaryConduit =$=
    recenterFixedSizeConduit parallelParams gaussianFilter =$=
    applyV4SeparableFilterLabeledArrayConduit parallelParams filterVecsList =$=
    CL.take 1
  let trainParams =
        TrainParams
        { trainSolver = L2R_L2LOSS_SVC_DUAL
        , trainC = 512
        , trainNumExamples = L.length featurePtr
        , trainFeatureIndexMax = VU.length . snd . L.head $ featurePtr1
            -- (L.sum . L.map filterNum $ filterVecsList) *
            -- if isColor
            --   then 3
            --   else 1
        , trainModel = modelName
        }
      (labels, features) = L.unzip featurePtr
  print trainParams
  print . VU.length . snd . L.head $ featurePtr1
  train trainParams labels features
