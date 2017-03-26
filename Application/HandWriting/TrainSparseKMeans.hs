import           Application.HandWriting.Conduit
import           Application.HandWriting.IO
import           CV.Statistics.KMeans
import           Classifier.LibLinear
import           Control.Arrow
import           Control.Monad                                as M
import           Control.Monad.Trans.Resource
import           CV.Utility.Parallel
import           CV.V4Filter                                  hiding (applyFilterVariedSizeConduit,
                                                               applyV4QuardTreeFilterConduit)
import           Data.Binary
import           Data.Conduit
import           Data.Conduit.Binary                          as CB
import           Data.Conduit.List                            as CL
import           Data.IntMap.Strict                           as IM
import           Data.List                                    as L
import           Data.Set                                     as S
import           Data.Vector                                  as V
import           Data.Vector.Unboxed                          as VU
import           Data.Word
import           System.Environment
import           System.IO

main = do
  (path:filterParamsPath:kmeansFile:modelName:cStr:numThreadStr:numBatchSizeStr:_) <-
    getArgs
  v4QuardTreeFilterParams <- fmap read . readFile $ filterParamsPath
  let parallelParams =
        ParallelParams
        { numThread = read numThreadStr :: Int
        , batchSize = read numBatchSizeStr :: Int
        }
      filterVecsList = generateV4FilterQuardTreeFilter v4QuardTreeFilterParams
  kmeansModel <- decodeFile kmeansFile :: IO [KMeansModel]
  labelFeaturePtr <-
    runResourceT $
    CB.sourceFile path $$ sparseOfflineCharacterConduit =$=
    applyV4QuardTreeFilterConduit parallelParams filterVecsList =$=
    kmeansConduit parallelParams kmeansModel =$=
    CL.map (second $ normalizeVec . VU.concat . L.map VU.concat) =$=
    featurePtrConduitP parallelParams =$=
    CL.consume
  let (labels, xs) = L.unzip labelFeaturePtr
      trainParams =
        TrainParams
        { trainSolver = L2R_L2LOSS_SVC_DUAL
        , trainC = read cStr :: Double
        , trainNumExamples = L.length xs
        , trainFeatureIndexMax =
            L.sum $
            L.zipWith
              (\a b -> a * b)
              (L.map (V.length . center) kmeansModel)
              (L.map L.length $ filterVecsList) 
        , trainModel = modelName
        }
  print trainParams
  train trainParams labels xs
