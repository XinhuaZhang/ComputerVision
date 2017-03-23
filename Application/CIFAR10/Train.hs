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
  (imageListPath:labelListPath:isColorStr:gridSizeStr:_) <- getArgs
  let parallelParams =
        ParallelParams
        { numThread = 16
        , batchSize = 3200
        }
      polarSeparableFilterParams =
        PolarSeparableFilterParamsGrid
        { getPolarSeparableFilterGridRows = fst gridSize
        , getPolarSeparableFilterGridCols = snd gridSize
        , getPolarSeparableFilterRows = n
        , getPolarSeparableFilterCols = n
        , getPolarSeparableFilterDownsampleFactor = downsampleFactor
        , getPolarSeparableFilterScale = [8]
        , getPolarSeparableFilterRadialFreq = [0 .. (8 - 1)]
        , getPolarSeparableFilterAngularFreq = [0 .. (8 - 1)]
        , getPolarSeparableFilterName = Pinwheels
        }
      cartesianGratingFilterParams =
        CartesianGratingFilterParams
        { getCartesianGratingFilterGridRows = fst gridSize
        , getCartesianGratingFilterGridCols = snd gridSize
        , getCartesianGratingFilterRows = n
        , getCartesianGratingFilterCols = n
        , getCartesianGratingFilterDownsampleFactor = downsampleFactor
        , getCartesianGratingFilterScale = [16]
        , getCartesianGratingFilterFreq = [0.125, 0.25, 0.5, 1]
        , getCartesianGratingFilterAngle = [0,10 .. 180 - 10] -- [0, 45, 90, 135]
        }
      hyperbolicFilterParams =
        HyperbolicFilterParams
        { getHyperbolicFilterGridRows = fst gridSize
        , getHyperbolicFilterGridCols = snd gridSize
        , getHyperbolicFilterRows = n
        , getHyperbolicFilterCols = n
        , getHyperbolicFilterDownsampleFactor = downsampleFactor
        , getHyperbolicFilterScale = [16]
        , getHyperbolicFilterFreq = [0.125, 0.25, 0.5, 1, 1.5]
        , getHyperbolicFilterAngle = [0,10 .. 90 - 10] --[0, 45, 90]
        }
      polarSeparableFilter =
        getFilterVectors
          (makeFilter $ PolarSeparableFilter polarSeparableFilterParams [] :: PolarSeparableFilterExpansion)
      cartesianGratingFilter =
        getFilterVectors
          (makeFilter $ CartesianGratingFilter cartesianGratingFilterParams [] :: CartesianGratingFilter)
      hyperbolicFilter =
        getFilterVectors
          (makeFilter $ HyperbolicFilter hyperbolicFilterParams [] :: HyperbolicFilter)
      filters =
        L.zipWith3
          (\a b c -> a L.++ b L.++ c)
          polarSeparableFilter
          cartesianGratingFilter
          hyperbolicFilter
      n = 64
      downsampleFactor = 1
      isColor = read isColorStr :: Bool
      gridSize = read gridSizeStr :: (Int, Int)
  labels <- readLabelFile labelListPath
  featurePtr <-
    runResourceT $
    imagePathSource imageListPath $$ readImageConduit isColor =$=
    resizeImageConduit parallelParams n =$=
    applyFilterFixedSizeConduit parallelParams downsampleFactor filters =$=
    CL.map VU.concat =$=
    featurePtrConduitP parallelParams =$=
    CL.consume
  let trainParams =
        TrainParams
        { trainSolver = L2R_L2LOSS_SVC_DUAL
        , trainC = 32
        , trainNumExamples = L.length featurePtr
        , trainFeatureIndexMax =
          (getFilterSize
             (PolarSeparableFilter polarSeparableFilterParams [] :: PolarSeparableFilterExpansion) +
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
