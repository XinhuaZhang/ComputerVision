import           Application.RotateDataset.RotationRepa
import           Control.Monad                          as M
import           Control.Monad.Trans.Resource
import           CV.Array.Image
import           CV.IO.ImageIO
import           CV.Utility.Parallel
import           Data.Conduit
import           Data.Conduit.List                      as CL
import           Data.List                              as L
import           Data.Vector.Unboxed                    as VU
import           System.Environment

main = do
  (imageListPath:isColorStr:_) <- getArgs
  let parallelParams = ParallelParams {numThread = 4, batchSize = 4}
  imgs <-
    runResourceT $
    imagePathSource imageListPath $$ readImageConduit (read isColorStr :: Bool) =$=
    resizeImageConduit parallelParams 128 =$=
    CL.take 10
  M.zipWithM_ (\i im -> plotImage (show i L.++ ".png") im) [1..] imgs
