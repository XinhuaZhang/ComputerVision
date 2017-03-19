import           Application.FacialExpression.Conduit
import           Application.RotateDataset.RotationRepa
import           Classifier.LibLinear
import           Control.Monad                        as M
import           Control.Monad.Trans.Resource
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
  (imageListPath:labelListPath:isColorStr:_) <- getArgs
  let parallelParams =
        ParallelParams
        { numThread = 16
        , batchSize = 3200
        }
      polarSeparableFilterParamsSet =
        PolarSeparableFilterParamsSet
        { getSizeSet = (n, n)
        , getDownsampleFactorSet = downsampleFactor
        , getScaleSet = S.fromDistinctAscList [4,6,8] --[1.5, 2, 3, 4] -- [4,6,8]
        , getRadialFreqSet = S.fromDistinctAscList [0 .. (6 - 1)] -- 6x6
        , getAngularFreqSet = S.fromDistinctAscList [0 .. (6 - 1)]
        , getNameSet = Pinwheels
        }
      cartesianGratingFilterParams =
        CartesianGratingFilterParams
        { getCartesianGratingFilterRows = n
        , getCartesianGratingFilterCols = n
        , getCartesianGratingFilterDownsampleFactor = downsampleFactor
        , getCartesianGratingFilterScale = [4, 8, 16]
        , getCartesianGratingFilterFreq = [0.125, 0.25, 0.5, 1]
        , getCartesianGratingFilterAngle = [0,10 .. 360 - 10]
        }
      hyperbolicFilterParams =
        HyperbolicFilterParams
        { getHyperbolicFilterRows = n
        , getHyperbolicFilterCols = n
        , getHyperbolicFilterDownsampleFactor = downsampleFactor
        , getHyperbolicFilterScale = [4, 8, 16] -- [4,8,16]
        , getHyperbolicFilterFreq = [0.5, 1, 1.5]
        , getHyperbolicFilterAngle = [0,10 .. 90 - 10]
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
      n = 64
      downsampleFactor = 1
      isColor = read isColorStr :: Bool
  labels <- readLabelFile labelListPath
  featurePtr <-
    runResourceT $
    imagePathSource imageListPath $$ readImageConduit isColor =$=
    resizeImageConduit parallelParams n =$=
    applyFilterFixedSizeConduit parallelParams downsampleFactor filters =$=
    featurePtrConduitP parallelParams =$=
    CL.consume
  let trainParams =
        TrainParams
        { trainSolver = L2R_L2LOSS_SVC_DUAL
        , trainC = 32
        , trainNumExamples = L.length featurePtr
        , trainFeatureIndexMax =
          (getFilterSize
             (PolarSeparableFilter polarSeparableFilterParamsSet [] :: PolarSeparableFilterExpansion) +
           getFilterSize
             (CartesianGratingFilter cartesianGratingFilterParams [] :: CartesianGratingFilter) +
           getFilterSize
             (HyperbolicFilter hyperbolicFilterParams [] :: HyperbolicFilter)) *
          if isColor
            then 6
            else 2
        , trainModel = "SVM_model"
        }
  print trainParams
  -- bestC <- findParameterC trainParams labels featurePtr
  -- let trainParams1 =
  --       TrainParams
  --       { trainSolver = L2R_L2LOSS_SVC_DUAL
  --       , trainC = bestC
  --       , trainNumExamples = L.length featurePtr
  --       , trainFeatureIndexMax =
  --         (getFilterSize
  --            (PolarSeparableFilter polarSeparableFilterParamsSet [] :: PolarSeparableFilterExpansion) +
  --          getFilterSize
  --            (CartesianGratingFilter cartesianGratingFilterParams [] :: CartesianGratingFilter) +
  --          getFilterSize
  --            (HyperbolicFilter hyperbolicFilterParams [] :: HyperbolicFilter)) *
  --         if isColor
  --           then 6
  --           else 2
  --       , trainModel = "SVM_model"
  --       }
  train trainParams labels featurePtr
