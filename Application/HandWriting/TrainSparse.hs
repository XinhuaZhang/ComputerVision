import           Application.HandWriting.Conduit
import           Application.HandWriting.IO
import           Classifier.LibLinear
import           Control.Arrow
import           Control.Monad.Trans.Resource
import           CV.Utility.Parallel
import           CV.V4Filter                     hiding
                                                  (applyFilterVariedSizeConduit,
                                                  applyV4QuardTreeFilterConduit)
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
  let parallelParams = ParallelParams {numThread = 12, batchSize = 4800}
      v4QuardTreeFilterParams =
        V4FilterQuardTreeFilterParams
        { quardTreeLayer = 2
        , rows = n
        , cols = n
        , polarSeparableFilterScale = [16]
        , polarSeparableFilterRadialFreq = [16, 8, 4]
        , polarSeparableFilterAngularFreq = [8, 8, 4]
        , polarSeparableFilterName = Pinwheels
        , cartesianGratingFilterScale = [24]
        , cartesianGratingFilterFreq = [0.125, 0.25, 0.5, 1]
        , cartesianGratingFilterAngle = 10
        , hyperbolicFilterFilterScale = [24]
        , hyperbolicFilterFilterFreq = [0.125, 0.25, 0.5, 1]
        , hyperbolicFilterFilterAngle = 10
        }
      filterVecsList = generateV4FilterQuardTreeFilter v4QuardTreeFilterParams
      n = 128
      downsampleFactor = 1
      gridSize = read gridSizeStr :: (Int, Int)
  labelFeaturePtr <-
    runResourceT $
    CB.sourceFile path $$ sparseOfflineCharacterConduit =$=
    applyV4QuardTreeFilterConduit parallelParams filterVecsList =$=
    CL.map (second $ VU.concat . L.map VU.concat) =$=
    featurePtrConduitP parallelParams =$=
    CL.consume
  let (labels, xs) = L.unzip labelFeaturePtr
      trainParams =
        TrainParams
        { trainSolver = L2R_L2LOSS_SVC_DUAL
        , trainC = 128
        , trainNumExamples = L.length xs
        , trainFeatureIndexMax =
            (L.sum . L.map (\ys -> (L.length . L.head $ ys) * L.length ys) $
             filterVecsList) *
            2
        , trainModel = modelName
        }
  print trainParams
  train trainParams labels xs
