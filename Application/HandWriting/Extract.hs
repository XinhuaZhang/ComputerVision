import           Application.HandWriting.Conduit
import           Application.HandWriting.IO
import           Control.Monad.Trans.Resource
import           CV.Utility.Parallel
import           Data.Conduit
import           Data.Conduit.Binary             as CB
import           Data.IntMap.Strict              as IM
import           Data.List                       as L
import           Data.Word
import           System.Environment

main = do
  (inputPath:outputPath:labelMapPath:newSizeStr:maxIndexStr:extractAllFlagStr:_) <-
    getArgs
  labelMapStr <- readFile labelMapPath
  let parallelParams =
        ParallelParams
        { numThread = 4
        , batchSize = 4800
        }
      labelMap = read labelMapStr :: IntMap Word16
  runResourceT $
    hwdbSource inputPath =$= offlineCharacterConduit labelMap $$
    if read extractAllFlagStr :: Bool
      then extractRangeConduit (1, read maxIndexStr :: Int) =$=
           rescaleConduit parallelParams (read newSizeStr :: Int) =$=
           sinkFile outputPath
      else rescaleConduit parallelParams (read newSizeStr :: Int) =$= sinkFile outputPath
