import           Application.HandWriting.Conduit
import           Application.HandWriting.IO
import           Classifier.LibLinear
import           Control.Arrow
import           Control.Monad.Trans.Resource
import           CV.Utility.Parallel
import           CV.V4Filter                     hiding
                                                  (applyFilterVariedSizeConduit,
                                                  applyV4QuadTreeFilterConduit)
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
  let parallelParams = ParallelParams {numThread = 32, batchSize = 6400}
      v4QuadTreeFilterParams =
        V4QuadTreeFilterParams
        { quadTreeLayer = 4
        , rows = n
        , cols = n
        , polarSeparableFilterScale = [16]
        , polarSeparableFilterRadialFreq = [16,10,8,6,4]
        , polarSeparableFilterAngularFreq = [8,8,8,6,4]
        , polarSeparableFilterName = Pinwheels
        , cartesianGratingFilterScale = [24]
        , cartesianGratingFilterFreq = [0.125, 0.25, 0.5, 1]
        , cartesianGratingFilterAngle = 10
        , hyperbolicFilterFilterScale = [24]
        , hyperbolicFilterFilterFreq = [0.125,0.25, 0.5, 1]
        , hyperbolicFilterFilterAngle = 10
        }
      -- v4QuadTreeFilterParams =
      --   V4QuadTreeFilterParams
      --   { quadTreeLayer = 1
      --   , rows = n
      --   , cols = n
      --   , polarSeparableFilterScale = [16]
      --   , polarSeparableFilterRadialFreq = [8]
      --   , polarSeparableFilterAngularFreq = [8]
      --   , polarSeparableFilterName = Pinwheels
      --   , cartesianGratingFilterScale = [24]
      --   , cartesianGratingFilterFreq = [0, 0.125, 0.5, 1]
      --   , cartesianGratingFilterAngle = 45
      --   , hyperbolicFilterFilterScale = [24]
      --   , hyperbolicFilterFilterFreq = [0, 0.125, 0.5, 1]
      --   , hyperbolicFilterFilterAngle = 45
      --   }
      filterVecsList = generateV4FilterQuadTreeFilter v4QuadTreeFilterParams
      -- filterVecsList = makeV4Filter v4QuadTreeFilterParams
      n = 128
      downsampleFactor = 1
      gridSize = read gridSizeStr :: (Int, Int)
  labelFeaturePtr <-
    runResourceT $
    CB.sourceFile path $$ sparseOfflineCharacterConduit =$=
    applyV4QuadTreeFilterConduit parallelParams filterVecsList =$=
    CL.map (second $ VU.concat . L.map VU.concat) =$=
    featurePtrConduitP parallelParams =$=
    CL.consume
  let (labels, xs) = L.unzip labelFeaturePtr
      trainParams =
        TrainParams
        { trainSolver = L2R_L2LOSS_SVC_DUAL
        , trainC = 1
        , trainNumExamples = L.length xs
        , trainFeatureIndexMax =
            (L.sum . L.map (\ys -> (L.length . L.head $ ys) * L.length ys) $
             filterVecsList) *
            2
        , trainModel = modelName
        }
  print trainParams
  train trainParams labels xs
