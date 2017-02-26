import           Application.Video.IO
import           Control.Monad                as M
import           Control.Monad.Trans.Resource
import           Data.Conduit
import           Data.Conduit.List            as CL

main = do
  frames <-
    runResourceT $
    videoFrameSource
      "/Users/xzhang/WorkSpace/ComputerVision/Application/Video/v_Bowling_g01_c01.avi" $$
    CL.take 2
  M.zipWithM_ (\i frame -> plotFrame (show i ++ ".png") frame) [1 ..] frames
