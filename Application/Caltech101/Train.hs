import           Application.Caltech101.Conduit
import           Application.RotateDataset.RotationRepa
import           Classifier.LibLinear
import           Control.Monad                          as M
import           Control.Monad.Trans.Resource
import           CV.Array.Image
import           CV.IO.ImageIO
import           CV.Utility.Parallel
import           CV.V4Filter
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
        , getScaleSet = S.fromDistinctAscList [6, 8, 10] --[8,16,24]
        , getRadialFreqSet = S.fromDistinctAscList [0 .. (8 - 1)]
        , getAngularFreqSet = S.fromDistinctAscList [0 .. (8 - 1)]
        , getNameSet = Pinwheels
        }
      cartesianGratingFilterParams =
        CartesianGratingFilterParams
        { getCartesianGratingFilterRows = n
        , getCartesianGratingFilterCols = n
        , getCartesianGratingFilterDownsampleFactor = downsampleFactor
        , getCartesianGratingFilterScale = [8, 16, 24] --[24, 48, 64]
        , getCartesianGratingFilterFreq = [0.1, 0.2, 0.4, 0.6, 0.8] -- [0.125, 0.25, 0.5]
        , getCartesianGratingFilterAngle = [0,10 .. 360 - 10]
        }
      hyperbolicFilterParams =
        HyperbolicFilterParams
        { getHyperbolicFilterRows = n
        , getHyperbolicFilterCols = n
        , getHyperbolicFilterDownsampleFactor = downsampleFactor
        , getHyperbolicFilterScale = [8, 16, 24] --[24, 48, 64]
        , getHyperbolicFilterFreq = [0.1, 0.2, 0.4, 0.6, 0.8, 1] -- [0.125, 0.25, 0.5, 1]
        , getHyperbolicFilterAngle = [0,10 .. 90 - 10]
        }
      n = 0
      downsampleFactor = 1
      maxSize = 100
      isColor = read isColorStr :: Bool
  labels <- runResourceT $ labelSource labelListPath $$ CL.consume
  xs <-
    runResourceT $
    imagePathSource imageListPath $$ readImageConduit isColor =$=
    resizeImageConduit parallelParams maxSize =$=
    applyFilterVariedSizeConduit
      parallelParams
      polarSeparableFilterParamsSet
      cartesianGratingFilterParams
      hyperbolicFilterParams =$=
    featurePtrConduit =$=
    CL.consume
  let trainParams =
        TrainParams
        { trainSolver = L2R_L2LOSS_SVC_DUAL
        , trainC = 1
        , trainNumExamples = L.length xs
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
  train trainParams labels xs
