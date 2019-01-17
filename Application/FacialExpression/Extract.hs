import           Application.FacialExpression.Conduit
import           CV.Statistics.PCA
import           Control.Monad                        as M
import           Control.Monad.Trans.Resource
import           CV.Array.Image
import           CV.IO.ImageIO
import           CV.Utility.Parallel
import           Data.Conduit
import           Data.Conduit.List                    as CL
import           Data.List                            as L
import           Data.Set                             as S
import           Data.Vector.Unboxed                  as VU
import           System.Environment
main = do
  (path:_) <- getArgs
  let parallelParams = ParallelParams {numThread = 4, batchSize = 100}
  labels <- runResourceT $ labelSource' path $$ CL.consume
  landmarks <- runResourceT $ landmarksSource path $$ CL.consume
  images <-
    runResourceT $
    filePathSource path $$ readImageConduit False =$=
    mergeSource (CL.sourceList landmarks) =$=
    cropSquareConduit parallelParams 128 =$=
    CL.take 10
  M.zipWithM
    (\i img -> plotImageRepa ("Face_" L.++ show i L.++ ".png") img)
    [1 ..]
    images
