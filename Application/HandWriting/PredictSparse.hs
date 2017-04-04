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
  (path:modelName:paramsFilePath:_) <- getArgs
  v4QuardTreeFilterParams <-
    fmap (\x -> read x :: V4QuadTreeSeparableFilterParams) . readFile $
    paramsFilePath
  let parallelParams = ParallelParams {numThread = 16, batchSize = 1600}
      filterVecsList =
        generateV4SeparableFilterQuadTreeFilter v4QuardTreeFilterParams
  runResourceT $
    CB.sourceFile path $$ sparseOfflineCharacterConduit =$=
    applyV4QuadTreeFilterConduit parallelParams filterVecsList =$=
    CL.map (second $ VU.concat . L.map VU.concat) =$=
    featureConduitP parallelParams =$=
    predict modelName (modelName L.++ ".out")
