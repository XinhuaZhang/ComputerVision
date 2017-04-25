import           Application.Leaf.Conduit
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
  let parallelParams =
        ParallelParams
        { numThread = 12
        , batchSize = 120
        }
      m = 45
      v4QuardTreeFilterParams =
        V4SeparableFilterParamsAxis
        { v4SeparableFilterParamsAxisSeparableFilterRows = rows
        , v4SeparableFilterParamsAxisSeparableFilterCols = cols
        , v4SeparableFilterParamsAxisPolarSeparablePolarFactor = 1
        , v4SeparableFilterParamsAxisPolarSeparableScale = [32]
        -- , v4SeparableFilterParamsAxisPolarSeparableRadialFreq = [0..7]
        -- , v4SeparableFilterParamsAxisPolarSeparableAngularFreq = [0..7]
        , v4SeparableFilterParamsAxisPolarSeparableFreq = [1..2]
        , v4SeparableFilterParamsAxisPolarSeparableAngle = [0]
        , v4SeparableFilterParamsAxisCartesianGratingScale =
          [ 2 ** (i / 2)
          | i <- [7 .. 10] ]
        , v4SeparableFilterParamsAxisCartesianGratingFreq = L.take 8 [1 .. 8]
        , v4SeparableFilterParamsAxisCartesianGratingAngle = [0,15 .. 360 - 15]
        , v4SeparableFilterParamsAxisHyperbolicSeparableScale =
          [ 2 ** (i / 2)
          | i <- [7 .. 10] ]
        , v4SeparableFilterParamsAxisHyperbolicSeparableUFreq = [0 .. 3]
        , v4SeparableFilterParamsAxisHyperbolicSeparableVFreq = [0 .. 3]
        , v4SeparableFilterParamsAxisHyperbolicSeparableAngle = 15
        , v4SeparableFilterParamsAxisSeparableFilterParams = P
        }
      (rows,cols) = read sizeStr :: (Int,Int)
      isColor = read isColorStr :: Bool
      gaussianFilterParams = GaussianFilterParams 32 rows cols
      gaussianFilter = Gaussian.makeFilter gaussianFilterParams
  writeFile paramsFilePath . show $ v4QuardTreeFilterParams
  featurePtr <-
    runResourceT $
    CB.sourceFile imageListPath $$ readLabeledImagebinaryConduit =$=
    applyV4SeparableFilterLabeledArrayWithCenterConduit
      parallelParams
      gaussianFilter
      v4QuardTreeFilterParams =$=
    featurePtrConduit =$=
    CL.consume
  featurePtr1 <-
    runResourceT $
    CB.sourceFile imageListPath $$ readLabeledImagebinaryConduit =$=
    applyV4SeparableFilterLabeledArrayWithCenterConduit
      parallelParams
      gaussianFilter
      v4QuardTreeFilterParams =$=
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
  train trainParams labels features
