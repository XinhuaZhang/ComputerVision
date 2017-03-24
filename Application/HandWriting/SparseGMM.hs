import           Application.HandWriting.Conduit
import           Application.HandWriting.IO
import           Application.MultiDimensionalGMM.GMM
import           Classifier.LibLinear
import           Control.Arrow
import           Control.Monad                       as M
import           Control.Monad.Trans.Resource
import           CV.Utility.Parallel
import           CV.V4Filter                         hiding (applyFilterVariedSizeConduit,
                                                      applyV4QuardTreeFilterConduit)
import           Data.Conduit
import           Data.Conduit.Binary                 as CB
import           Data.Conduit.List                   as CL
import           Data.IntMap.Strict                  as IM
import           Data.List                           as L
import           Data.Set                            as S
import           Data.Vector.Unboxed                 as VU
import           Data.Word
import           System.Environment
import           System.IO

main = do
  (path:numLayerStr:gmmFile:numGaussianStr:thresholdStr:numTrainStr:filterParamsPath:numThreadStr:numBatchSizeStr:_) <-
    getArgs
  let parallelParams =
        ParallelParams
        { numThread = read numThreadStr :: Int
        , batchSize = read numBatchSizeStr :: Int
        }
      v4QuardTreeFilterParams =
        V4FilterQuardTreeFilterParams
        { quardTreeLayer = read numLayerStr :: Int
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
  writeFile filterParamsPath . show $ v4QuardTreeFilterParams
  labelFeature <-
    runResourceT $
    CB.sourceFile path $$ sparseOfflineCharacterConduit =$=
    applyV4QuardTreeFilterConduit parallelParams filterVecsList =$=
    CL.consume
  withBinaryFile gmmFile WriteMode $
    \h ->
       M.mapM_
         (\feature ->
             runResourceT $
             sourceList feature $$
             hGMMSink1
               parallelParams
               h
               (read numGaussianStr :: Int)
               (read thresholdStr :: Double)
               (read numTrainStr :: Int)) .
       snd . L.unzip $
       labelFeature
