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
  let parallelParams =
        ParallelParams
        { numThread = 16
        , batchSize = 320
        }
      polarSeparableFilterParamsSet =
        PolarSeparableFilterParamsSet
        { getSizeSet = (n, n)
        , getDownsampleFactorSet = downsampleFactor
        , getScaleSet = S.fromDistinctAscList [16, 20, 24]
        , getRadialFreqSet = S.fromDistinctAscList [0 .. (16 - 1)]
        , getAngularFreqSet = S.fromDistinctAscList [0 .. (16 - 1)]
        , getNameSet = Pinwheels
        }
      cartesianGratingFilterParams =
        CartesianGratingFilterParams
        { getCartesianGratingFilterRows = n
        , getCartesianGratingFilterCols = n
        , getCartesianGratingFilterDownsampleFactor = downsampleFactor
        , getCartesianGratingFilterScale = [16, 32, 64]
        , getCartesianGratingFilterFreq = [0.1, 0.2, 0.4, 0.6, 0.8]
        , getCartesianGratingFilterAngle = [0,10 .. 360 - 10] -- [0, 45, 90, 135]
        }
      hyperbolicFilterParams =
        HyperbolicFilterParams
        { getHyperbolicFilterRows = n
        , getHyperbolicFilterCols = n
        , getHyperbolicFilterDownsampleFactor = downsampleFactor
        , getHyperbolicFilterScale = [16, 32, 64]
        , getHyperbolicFilterFreq = [0.1, 0.2, 0.4, 0.6, 0.8, 1]
        , getHyperbolicFilterAngle = [0,10 .. 90 - 10] --[0, 45, 90]
        }
      polarSeparableFilter =
        getFilterVectors
          (makeFilter $ PolarSeparableFilter polarSeparableFilterParamsSet [] :: PolarSeparableFilterExpansion)
      cartesianGratingFilter =
        getFilterVectors
          (makeFilter $ CartesianGratingFilter cartesianGratingFilterParams [] :: CartesianGratingFilter)
      hyperbolicFilter =
        getFilterVectors
          (makeFilter $ HyperbolicFilter hyperbolicFilterParams [] :: HyperbolicFilter)
      filters = [polarSeparableFilter, cartesianGratingFilter, hyperbolicFilter]
      n = 256
      downsampleFactor = 1
  labels <- runResourceT $ labelSource' path $$ CL.consume
  landmarks <- runResourceT $ landmarksSource path $$ CL.consume
  features <-
    runResourceT $
    filePathSource path $$ readImageConduit False =$=
    mergeSource (CL.sourceList landmarks) =$=
    cropSquareConduit parallelParams n =$=
    applyFilterFixedSizeConduit parallelParams downsampleFactor filters =$=
    CL.consume
  let trainParams =
        TrainParams
        { trainSolver = L2R_L2LOSS_SVC_DUAL
        , trainC = 1
        , trainNumExamples = L.length features
        , trainFeatureIndexMax = (L.sum . L.map L.length $ filters) * 2
        , trainModel = "SVM_model"
        }
  print trainParams
  crossValidation
    trainParams
    8
    (L.sum . L.map L.length $ filters)
    (L.zip (L.map fromIntegral labels) features)
