import           Application.HandWriting.Conduit
import           Application.HandWriting.IO
import           Classifier.LibLinear
import           Control.Arrow
import           Control.Monad.Trans.Resource
import           CV.Utility.Parallel
import           CV.V4Filter                     hiding
                                                  (applyFilterVariedSizeConduit,applyV4QuadTreeFilterConduit)
import           Data.Conduit
import           Data.Conduit.Binary             as CB
import           Data.Conduit.List               as CL
import           Data.IntMap.Strict              as IM
import           Data.List                       as L
import           Data.Set                        as S
import           Data.Vector.Unboxed             as VU
import           Data.Word
import           System.Environment

main = do
  (path:modelName:gridSizeStr:_) <- getArgs
  let parallelParams = ParallelParams {numThread = 32, batchSize = 3200}
      v4QuardTreeFilterParams =
        V4QuadTreeSeparableFilterParams
        { separableFilterQuadTreeLayer = 2
        , separableFilterRows = n
        , separableFilterCols = n
        , polarSeparableScale = [16]
        , polarSeparableRadialFreq = [16, 10, 8]
        , polarSeparableAngularFreq = [16, 10, 8]
        , polarSeparableName = Pinwheels
        , cartesianSeparableScale = [28]
        , cartesianSeparableXFreq = [0,0.05 .. 1]
        , cartesianSeparableYFreq = [0,0.05 .. 1]
        , hyperbolicSeparableScale = [28]
        , hyperbolicSeparableUFreq = [0,0.5 .. 4]
        , hyperbolicSeparableVFreq = [0,0.1 .. 1]
        , hyperbolicSeparableAngle = 15
        }
      filterVecsList =
        generateV4SeparableFilterQuadTreeFilter v4QuardTreeFilterParams
      -- v4QuadTreeFilterParams =
      --   V4QuadTreeFilterParams
      --   { quadTreeLayer = 4
      --   , rows = n
      --   , cols = n
      --   , polarSeparableFilterScale = [16]
      --   , polarSeparableFilterRadialFreq = [16,10,8,6,4]
      --   , polarSeparableFilterAngularFreq = [8,8,8,6,4]
      --   , polarSeparableFilterName = Pinwheels
      --   , cartesianGratingFilterScale = [24]
      --   , cartesianGratingFilterFreq = [0.125, 0.25, 0.5, 1]
      --   , cartesianGratingFilterAngle = 10
      --   , hyperbolicFilterFilterScale = [24]
      --   , hyperbolicFilterFilterFreq = [0.125,0.25, 0.5, 1]
      --   , hyperbolicFilterFilterAngle = 10
      --   }
      -- filterVecsList = generateV4FilterQuadTreeFilter v4QuadTreeFilterParams
      -- filterVecsList = makeV4Filter v4QuadTreeFilterParams
      n = 128
      downsampleFactor = 1
      gridSize = read gridSizeStr :: (Int, Int)
  runResourceT $
    CB.sourceFile path $$ sparseOfflineCharacterConduit =$=
    applyV4QuadTreeFilterConduit parallelParams filterVecsList =$=
    CL.map (second $ VU.concat . L.map VU.concat) =$=
    featureConduitP parallelParams =$=
    predict modelName (modelName L.++ ".out")
