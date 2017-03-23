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
  runResourceT $
    imagePathSource imageListPath $$ readImageConduit isColor =$=
    resizeImageConduit parallelParams n =$=
    applyFilterFixedSizeConduit parallelParams downsampleFactor filters =$=
    CL.map VU.concat =$=
    featureConduitP parallelParams =$=
    mergeSource (labelSource labelListPath) =$=
    predict "SVM_model" "SVM_model.out"
