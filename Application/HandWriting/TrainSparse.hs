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
  (path:modelName:cStr:paramsFilePath:patchSizeStr:_) <- getArgs
  let parallelParams = ParallelParams {numThread = 16, batchSize = 1600}
      v4QuardTreeFilterParams =
        V4QuadTreeSeparableFilterParams
        { separableFilterQuadTreeLayer = 1
        , separableFilterRows = n
        , separableFilterCols = n
        , polarSeparableScale = [32]
        , polarSeparableRadialFreq = [8, 8, 8]
        , polarSeparableAngularFreq = [8, 8, 8]
        , polarSeparableName = Pinwheels
        , cartesianSeparableScale = [36]
        , cartesianSeparableXFreq = [0..7]
        , cartesianSeparableYFreq = [0..7]
        , hyperbolicSeparableScale = [32]
        , hyperbolicSeparableUFreq = [0..3]
        , hyperbolicSeparableVFreq = [0..7]
        , hyperbolicSeparableAngle = 15
        , separableFilterParams = PCH
        }
      filterVecsList =
        generateV4SeparableFilterQuadTreeFilter v4QuardTreeFilterParams
      n = read patchSizeStr :: Int
      downsampleFactor = 1
  writeFile paramsFilePath . show $ v4QuardTreeFilterParams
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
        , trainC = read cStr :: Double
        , trainNumExamples = L.length xs
        , trainFeatureIndexMax =
            (L.sum . L.map (\ys -> (L.length . L.head $ ys) * L.length ys) $
             filterVecsList) *
            2
        , trainModel = modelName
        }
  print trainParams
  train trainParams labels xs
