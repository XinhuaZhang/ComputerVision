import           Application.FacialExpression.Conduit
import           Application.FacialExpression.PCA
import           Classifier.LibLinear
import           Control.Monad                        as M
import           Control.Monad.Trans.Resource
import           CV.Array.Image
import           CV.IO.ImageIO
import           CV.Utility.Parallel
import           CV.V4Filter
import           Data.Conduit
import           Data.Conduit.List                    as CL
import           Data.List                            as L
import           Data.Set                             as S
import           Data.Vector.Unboxed                  as VU
import           System.Environment

main = do
  (path:_) <- getArgs
  let parallelParams = ParallelParams {numThread = 12, batchSize = 480}
      v4QuardTreeFilterParams =
        V4QuadTreeSeparableFilterParams
        { separableFilterQuadTreeLayer = 1
        , separableFilterRows = n
        , separableFilterCols = n
        , polarSeparableScale = L.map (\i -> 8 * (2 ** (i / 2))) [4 .. 5 - 1]
        , polarSeparableRadialFreq = [8, 8, 8]
        , polarSeparableAngularFreq = [8, 8, 8]
        , polarSeparableName = Pinwheels
        , cartesianSeparableScale =
            L.map (\i -> 8 * (2 ** (i / 2))) [4 .. 5 - 1]
        , cartesianSeparableXFreq = [0 .. 7]
        , cartesianSeparableYFreq = [0 .. 7]
        , hyperbolicSeparableScale =
            L.map (\i -> 8 * (2 ** (i / 2))) [4 .. 5 - 1]
        , hyperbolicSeparableUFreq = [0 .. 3]
        , hyperbolicSeparableVFreq = [0 .. 7]
        , hyperbolicSeparableAngle = 90
        , separableFilterParams = PCH
        }
      filterVecsList =
        generateV4SeparableFilterQuadTreeFilter v4QuardTreeFilterParams
      n = 128
      downsampleFactor = 1
      gridSize = (16, 16)
  labels <- runResourceT $ labelSource' path $$ CL.consume
  landmarks <- runResourceT $ landmarksSource path $$ CL.consume
  features <-
    runResourceT $
    filePathSource path $$ readImageConduit False =$=
    mergeSource (CL.sourceList landmarks) =$=
    cropSquareConduit parallelParams n =$=
    applyV4QuadTreeFilterConduit parallelParams filterVecsList =$=
    CL.map (VU.concat . L.map VU.concat) =$=
    CL.consume
  let trainParams =
        TrainParams
        { trainSolver = L2R_L2LOSS_SVC_DUAL
        , trainC = 128
        , trainNumExamples = L.length features
        , trainFeatureIndexMax =
            (L.sum . L.map (\ys -> (L.length . L.head $ ys) * L.length ys) $
             filterVecsList) *
            2
        , trainModel = "SVM_model"
        }
  print trainParams
  crossValidation
    parallelParams
    trainParams
    8
    (L.zip (L.map fromIntegral labels)  features)
