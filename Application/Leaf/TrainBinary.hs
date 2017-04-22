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
      m = 15
      v4QuardTreeFilterParams =
        V4SeparableFilterParamsGrid
        { v4SeparableFilterParamsGridSeparableFilterRows = rows
        , v4SeparableFilterParamsGridSeparableFilterCols = cols
        -- , v4SeparableFilterParamsGridPolarSeparablePolarFactor = 1
        , v4SeparableFilterParamsGridPolarSeparableScale = [64]
        , v4SeparableFilterParamsGridPolarSeparableRadialFreq = [0..7]
        , v4SeparableFilterParamsGridPolarSeparableAngularFreq = [0..7]
        -- , v4SeparableFilterParamsGridPolarSeparableFreq = [-15,-14 .. 15]
        -- , v4SeparableFilterParamsGridPolarSeparableAngle = [0,m.. 90-m]
        , v4SeparableFilterParamsGridCartesianGratingScale =
          [ 2 ** (i / 2)
          | i <- [7 .. 10] ]
        , v4SeparableFilterParamsGridCartesianGratingFreq = L.take 8 [1 .. 8]
        , v4SeparableFilterParamsGridCartesianGratingAngle = [0,15 .. 360 - 15]
        , v4SeparableFilterParamsGridHyperbolicSeparableScale =
          [ 2 ** (i / 2)
          | i <- [7 .. 10] ]
        , v4SeparableFilterParamsGridHyperbolicSeparableUFreq = [0 .. 3]
        , v4SeparableFilterParamsGridHyperbolicSeparableVFreq = [0 .. 3]
        , v4SeparableFilterParamsGridHyperbolicSeparableAngle = 15
        , v4SeparableFilterParamsGridSeparableFilterParams = P
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
        , trainC = 1
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
