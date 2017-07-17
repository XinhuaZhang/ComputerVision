import           Application.FacialExpression.Conduit
import           Application.FacialExpression.PCA
import           Classifier.LibLinear
import           Control.Monad                        as M
import           Control.Monad.Trans.Resource
import           CV.Filter.PinwheelRing
import           CV.IO.ImageIO
import           CV.Utility.Parallel
import           Data.Conduit
import           Data.Conduit.List                    as CL
import           Data.List                            as L
import           Data.Vector.Unboxed                  as VU
import           System.Environment

main = do
  (path:_) <- getArgs
  let parallelParams =
        ParallelParams
        { numThread = 16
        , batchSize = 320
        }
      n = 128
      filterParams =
        PinwheelRingParams
        { pinwheelRingRows = n
        , pinwheelRingCols = n
        , pinwheelGaussianScale = 0.1
        , pinwheelRingScale = L.map (\x -> 2 ** x) [0 .. 1]
        , pinwheelRingRadialFreqs = 3 / 4 * pi
        , pinwheelRingAngularFreqs = [0 .. 15]
        , pinwheelRingRadius = [1 .. 48]
        }
      filters =
        [ makeFilterExpansion filterParams (div n 2) (div n 2) :: PinwheelRingExpansion
        ]
  labels <- runResourceT $ labelSource' path $$ CL.consume
  landmarks <- runResourceT $ landmarksSource path $$ CL.consume
  features <-
    runResourceT $
    filePathSource path $$ readImageConduit False =$=
    mergeSource (CL.sourceList landmarks) =$=
    cropSquareConduit parallelParams n =$=
    filterExpansionConduit parallelParams filters =$=
    CL.consume
  let trainParams =
        TrainParams
        { trainSolver = L2R_L2LOSS_SVC_DUAL
        , trainC = 512
        , trainNumExamples = L.length features
        , trainFeatureIndexMax = L.sum . L.map getFilterExpansionNum $ filters
        , trainModel = "SVM_model"
        }
  print trainParams
  crossValidation
    parallelParams
    trainParams
    8
    (L.zip (L.map fromIntegral labels) features)
