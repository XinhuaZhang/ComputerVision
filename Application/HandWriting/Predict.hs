import           Application.HandWriting.Conduit
import           Application.HandWriting.IO
import           Classifier.LibLinear
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

main =
  do (path:labelMapPath:modelName:maxIndexStr:gridSizeStr:_) <- getArgs
     labelMapStr <- readFile labelMapPath
     let parallelParams =
           ParallelParams {numThread = 32
                          ,batchSize = 6400}
         polarSeparableFilterParamsSet =
           PolarSeparableFilterParamsSet {getSizeSet = (n,n)
                                         ,getDownsampleFactorSet =
                                            downsampleFactor
                                         ,getScaleSet =
                                            S.fromDistinctAscList [4,6,8]
                                         ,getRadialFreqSet =
                                            S.fromDistinctAscList [0 .. (6 - 1)]
                                         ,getAngularFreqSet =
                                            S.fromDistinctAscList [0 .. (6 - 1)]
                                         ,getNameSet = Pinwheels}
         cartesianGratingFilterParams =
           CartesianGratingFilterParams {getCartesianGratingFilterRows = n
                                        ,getCartesianGratingFilterCols = n
                                        ,getCartesianGratingFilterDownsampleFactor =
                                           downsampleFactor
                                        ,getCartesianGratingFilterScale =
                                           [4,8,16]
                                        ,getCartesianGratingFilterFreq =
                                           [0.5,1]
                                        ,getCartesianGratingFilterAngle =
                                           [0,10 .. 180 - 10] -- [0, 45, 90, 135]
                                         }
         hyperbolicFilterParams =
           HyperbolicFilterParams {getHyperbolicFilterRows = n
                                  ,getHyperbolicFilterCols = n
                                  ,getHyperbolicFilterDownsampleFactor =
                                     downsampleFactor
                                  ,getHyperbolicFilterScale = [4,8,16]
                                  ,getHyperbolicFilterFreq = [0.25,0.5,1,1.5]
                                  ,getHyperbolicFilterAngle = [0,10 .. 90 - 10] --[0, 45, 90]
                                   }
         n = 0
         gridSize = read gridSizeStr :: (Int,Int)
         downsampleFactor = 1
         labelMap = read labelMapStr :: IntMap Word16
     runResourceT $
       hwdbSource path $$ offlineCharacterConduit labelMap =$=
       extractRangeConduit (1,read maxIndexStr :: Int) =$=
       applyFilterVariedSizeGridConduit parallelParams
                                        gridSize
                                        polarSeparableFilterParamsSet
                                        cartesianGratingFilterParams
                                        hyperbolicFilterParams =$=
       featureConduitP parallelParams =$=
       predict modelName (modelName L.++ ".out")
