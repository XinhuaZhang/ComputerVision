import           Application.Caltech101.Conduit
import           Application.RotateDataset.RotationRepa
import           Classifier.LibLinear
import           Control.Monad                          as M
import           Control.Monad.Trans.Resource
import           CV.Array.Image
import           CV.Filter.CartesianGratingFilter       as CF
import           CV.Filter.HyperbolicFilter             as HF
import           CV.Filter.PolarSeparableFilter         as PF
import           CV.IO.ImageIO
import           CV.Utility.Parallel
import           Data.Conduit
import           Data.Conduit.List                      as CL
import           Data.List                              as L
import           Data.Set                               as S
import           Data.Vector.Unboxed                    as VU
import           System.Environment

main = do
  (imageListPath:labelListPath:isColorStr:_) <- getArgs
  let parallelParams =
        ParallelParams
        { numThread = 16
        , batchSize = 320
        }
      polarSeparableFilterParamsSet =
        PolarSeparableFilterParamsSet
        { getSizeSet = (n, n)
        , getDownsampleFactorSet = downsampleFactor
        , getScaleSet = S.fromDistinctAscList [8,16,24]
        , getRadialFreqSet = S.fromDistinctAscList [0 .. (16 - 1)]
        , getAngularFreqSet = S.fromDistinctAscList [0 .. (16 - 1)]
        , getNameSet = Pinwheels
        }
      cartesianGratingFilterParams =
        CartesianGratingFilterParams
        { getCartesianGratingFilterSize = (n, n)
        , getCartesianGratingFilterDownsampleFactor = downsampleFactor
        , getCartesianGratingFilterScale = [24, 48, 64]
        , getCartesianGratingFilterFreq = [0.125, 0.25, 0.5]
        , getCartesianGratingFilterAngle = [0,10 .. 360 - 10]
        }
      hyperbolicFilterParams =
        HyperbolicFilterParams
        { getHyperbolicFilterSize = (n, n)
        , getHyperbolicFilterDownsampleFactor = downsampleFactor
        , getHyperbolicFilterScale = [24, 48, 64]
        , getHyperbolicFilterFreq = [0.125, 0.25, 0.5, 1]
        , getHyperbolicFilterAngle = [0,10 .. 90 - 10]
        }
      n = 0
      downsampleFactor = 1
      maxSize = 100
      isColor = read isColorStr :: Bool
  labels <-  runResourceT $ labelSource labelListPath $$ CL.consume
  xs <-
    runResourceT $
    imagePathSource imageListPath $$ readImageConduit isColor =$=
    resizeImageConduit parallelParams maxSize =$=
    applyFilterCenterVariedSizeConduit
      parallelParams
      polarSeparableFilterParamsSet
      cartesianGratingFilterParams
      hyperbolicFilterParams =$=
    featurePtrConduit =$=
    CL.consume
  let trainParams =
        TrainParams
        { trainSolver = L2R_L2LOSS_SVC_DUAL
        , trainC = 128
        , trainNumExamples = L.length xs
        , trainFeatureIndexMax =
          (PF.getFilterNum polarSeparableFilterParamsSet + CF.getFilterNum cartesianGratingFilterParams +
           HF.getFilterNum hyperbolicFilterParams) *
          if isColor
             then 6
             else 2
        , trainModel = "SVM_model"
        }
  train trainParams labels xs
