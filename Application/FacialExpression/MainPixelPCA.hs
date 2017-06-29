import           Application.FacialExpression.Conduit
import           Application.FacialExpression.PCA
import           Classifier.LibLinear
import           Control.Monad                        as M
import           Control.Monad.Trans.Resource
import           CV.Array.Image
import           CV.Filter.PolarSeparableFilter
import           CV.IO.ImageIO
import           CV.Utility.Parallel
import           Data.Conduit
import           Data.Conduit.List                    as CL
import           Data.List                            as L
import           Data.Set                             as S
import           Data.Vector.Unboxed                  as VU
import           System.Environment

main = do
  (path:_) <- getArgs
  let parallelParams =
        ParallelParams
        { numThread = 8
        , batchSize = 320
        }
      n = 128
  labels <- runResourceT $ labelSource' path $$ CL.consume
  landmarks <- runResourceT $ landmarksSource path $$ CL.consume
  features <-
    runResourceT $
    filePathSource path $$ readImageConduit False =$=
    mergeSource (CL.sourceList landmarks) =$=
    cropSquareConduit parallelParams n =$=
    pixelConduit parallelParams 1 =$=
    CL.consume
  let n = 454
      trainParams =
        TrainParams
        { trainSolver = L2R_L2LOSS_SVC_DUAL
        , trainC = 256
        , trainNumExamples = 0
        , trainFeatureIndexMax = n
        , trainModel = "SVM_model"
        }
  print trainParams
  print . L.length $ features
  print . VU.length . L.head $ features
  crossValidationPCA
    parallelParams
    trainParams
    8
    n
    (L.zip (L.map fromIntegral labels) features)