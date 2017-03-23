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
  (imageListPath:labelListPath:isColorStr:gridSizeStr:_) <- getArgs
  let parallelParams =
        ParallelParams
        { numThread = 16
        , batchSize = 320
        }
      polarSeparableFilterParams =
        PolarSeparableFilterParamsGrid
        { getPolarSeparableFilterGridRows = fst gridSize
        , getPolarSeparableFilterGridCols = snd gridSize
        , getPolarSeparableFilterRows = n
        , getPolarSeparableFilterCols = n
        , getPolarSeparableFilterDownsampleFactor = downsampleFactor
        , getPolarSeparableFilterScale = [16]
        , getPolarSeparableFilterRadialFreq = [0 .. (16 - 1)]
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
        , getCartesianGratingFilterScale = [24]
        , getCartesianGratingFilterFreq = [0.125, 0.25, 0.5, 1]
        , getCartesianGratingFilterAngle = [0,10 .. 180 - 10]
        }
      hyperbolicFilterParams =
        HyperbolicFilterParams
        { getHyperbolicFilterGridRows = fst gridSize
        , getHyperbolicFilterGridCols = snd gridSize
        , getHyperbolicFilterRows = n
        , getHyperbolicFilterCols = n
        , getHyperbolicFilterDownsampleFactor = downsampleFactor
        , getHyperbolicFilterScale = [24]
        , getHyperbolicFilterFreq = [0.125, 0.25, 0.5, 1]
        , getHyperbolicFilterAngle = [0,10 .. 90 - 10] --[0, 45, 90]
        }
      gridSize = read gridSizeStr :: (Int, Int)
      n = 0
      downsampleFactor = 1
      maxSize = 100
      isColor = read isColorStr :: Bool
  labels <- runResourceT $ labelSource labelListPath $$ CL.consume
  runResourceT $
    imagePathSource imageListPath $$ readImageConduit isColor =$=
    resizeImageConduit parallelParams maxSize =$=
    applyFilterVariedSizeConduit
      parallelParams
      polarSeparableFilterParams
      cartesianGratingFilterParams
      hyperbolicFilterParams =$=
    CL.map VU.concat =$=
    featureConduit =$=
    mergeSource (labelSource labelListPath) =$=
    predict "SVM_model" "SVM_model.out"
