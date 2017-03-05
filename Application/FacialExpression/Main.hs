import           Application.FacialExpression.Conduit
import           Classifier.LibLinear
import           CV.Filter.PolarSeparableFilter
import           CV.IO.ImageIO
import           CV.Utility.Parallel
import           Data.Conduit
import           Data.Conduit.List                    as CL
import           Data.List                            as L
import           Data.Set                             as S
import           Data.Vector.Unboxed                  as VU
import           System.Environment
import           Control.Monad.Trans.Resource

main = do
  (path:_) <- getArgs
  let parallelParams =
        ParallelParams
        { numThread = 16
        , batchSize = 160
        }
      filterParamsSet =
        PolarSeparableFilterParamsSet
        { getSizeSet = (0, 0)
        , getDownsampleFactorSet = 1
        , getScaleSet = S.fromDistinctAscList [2, 4, 8, 16]
        , getRadialFreqSet = S.fromDistinctAscList [0 .. (8 - 1)]
        , getAngularFreqSet = S.fromDistinctAscList [0 .. (8 - 1)]
        , getNameSet = Pinwheels
        }
      trainParams =
        TrainParams
        { trainSolver = L2R_L2LOSS_SVC_DUAL
        , trainC = 0.125
        , trainNumExamples = 327
        , trainFeatureIndexMax = getFilterNum filterParamsSet
        , trainModel = "SVM_model"
        }
  labels <- runResourceT $ labelSource' path $$ CL.consume
  featurePtr <-
    runResourceT $
    filePathSource path $$ readImageConduit False =$=
    applyFilterCenterVariedSizeConduit parallelParams filterParamsSet =$=
    CL.map complexVec2RealVec =$=
    featurePtrConduit =$=
    CL.consume
  findParameterC trainParams (L.map fromIntegral labels) featurePtr
