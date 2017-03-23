import           Application.HandWriting.Conduit
import           Application.HandWriting.IO
import           Classifier.LibLinear
import           Control.Arrow
import           Control.Monad.Trans.Resource
import           CV.Utility.Parallel
import           CV.V4Filter                     hiding
                                                  (applyFilterVariedSizeConduit)
import           Data.Conduit
import           Data.Conduit.List               as CL
import           Data.IntMap.Strict              as IM
import           Data.List                       as L
import           Data.Set                        as S
import           Data.Vector.Unboxed             as VU
import           Data.Word
import           System.Environment

main = do
  (path:labelMapPath:modelName:maxIndexStr:gridSizeStr:_) <- getArgs
  labelMapStr <- readFile labelMapPath
  let parallelParams =
        ParallelParams
        { numThread = 32
        , batchSize = 6400
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
        , getCartesianGratingFilterScale = [4, 8, 16]
        , getCartesianGratingFilterFreq = [0.5, 1]
        , getCartesianGratingFilterAngle = [0,10 .. 180 - 10] -- [0, 45, 90, 135]
        }
      hyperbolicFilterParams =
        HyperbolicFilterParams
        { getHyperbolicFilterGridRows = fst gridSize
        , getHyperbolicFilterGridCols = snd gridSize
        , getHyperbolicFilterRows = n
        , getHyperbolicFilterCols = n
        , getHyperbolicFilterDownsampleFactor = downsampleFactor
        , getHyperbolicFilterScale = [4, 8, 16]
        , getHyperbolicFilterFreq = [0.25, 0.5, 1, 1.5]
        , getHyperbolicFilterAngle = [0,10 .. 90 - 10] --[0, 45, 90]
        }
      n = 0
      gridSize = read gridSizeStr :: (Int, Int)
      downsampleFactor = 1
      labelMap = read labelMapStr :: IntMap Word16
  runResourceT $hwdbSource path $$ offlineCharacterConduit labelMap =$=
    extractRangeConduit (1, read maxIndexStr :: Int) =$=
    applyFilterVariedSizeGridConduit
      parallelParams
      polarSeparableFilterParams
      cartesianGratingFilterParams
      hyperbolicFilterParams =$=
    CL.map (second VU.concat) =$=
    featureConduitP parallelParams =$=
    predict modelName (modelName L.++ ".out")
