import           Application.FacialExpression.Conduit
import           Classifier.LibLinear
import           Control.Monad                        as M
import           Control.Monad.Trans.Resource
import           CV.Array.Image
import           CV.Filter.CartesianGratingFilter     as CF
import           CV.Filter.HyperbolicFilter           as HF
import           CV.Filter.PolarSeparableFilter       as PSF
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
        { numThread = 16
        , batchSize = 320
        }
      polarSeparableFilterParamsSet =
        PolarSeparableFilterParamsSet
        { getSizeSet = (n, n)
        , getDownsampleFactorSet = downsampleFactor
        , getScaleSet = S.fromDistinctAscList [4, 8, 16]
        , getRadialFreqSet = S.fromDistinctAscList [0 .. (16 - 1)]
        , getAngularFreqSet = S.fromDistinctAscList [0 .. (16 - 1)]
        , getNameSet = Pinwheels
        }
      (PolarSeparableFilter _ polarSeparableFilter) = makeCenterFilterSet polarSeparableFilterParamsSet
      cartesianGratingFilterParams =
        CartesianGratingFilterParams
        { getCartesianGratingFilterSize = (n, n)
        , getCartesianGratingFilterDownsampleFactor = downsampleFactor
        , getCartesianGratingFilterScale = [12, 18, 24]
        , getCartesianGratingFilterFreq = [0.125, 0.25, 0.5]
        , getCartesianGratingFilterAngle = [0,10..360-10] -- [0, 45, 90, 135]
        }
      (CartesianGratingFilter _ cartesianGratingFilter) = CF.makeFilter cartesianGratingFilterParams
      hyperbolicFilterParams =
        HyperbolicFilterParams
        { getHyperbolicFilterSize = (n, n)
        , getHyperbolicFilterDownsampleFactor = downsampleFactor
        , getHyperbolicFilterScale = [12, 18, 24]
        , getHyperbolicFilterFreq = [0.125,0.25,0.5,1]
        , getHyperbolicFilterAngle = [0,10..90-10]--[0, 45, 90]
        }
      (HyperbolicFilter _ hyperbolicFilter) = HF.makeFilter hyperbolicFilterParams
      filters = [polarSeparableFilter,cartesianGratingFilter,hyperbolicFilter]
      n = 128
      downsampleFactor = 1
  labels <- runResourceT $ labelSource' path $$ CL.consume
  landmarks <- runResourceT $ landmarksSource path $$ CL.consume
  featurePtr <-
    runResourceT $
    filePathSource path $$ readImageConduit False =$=
    mergeSource (CL.sourceList landmarks) =$=
    -- cropConduit parallelParams =$=
    cropSquareConduit parallelParams n =$=
    applyFilterCenterFixedSizeConduit
      parallelParams
      downsampleFactor
      filters =$=
    -- applyFilterCenterVariedSizeConduit parallelParams filterParamsSet =$=
    featurePtrConduitP parallelParams =$=
    CL.consume
  let trainParams =
        TrainParams
        { trainSolver = L2R_L2LOSS_SVC_DUAL
        , trainC = 0.125
        , trainNumExamples = L.length featurePtr
        , trainFeatureIndexMax =
          (L.sum . L.map L.length $ filters) * 2
        , trainModel = "SVM_model"
        }
  print trainParams
  findParameterC trainParams (L.map fromIntegral labels) featurePtr
