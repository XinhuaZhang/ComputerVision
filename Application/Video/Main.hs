import           Application.Video.IO
import           Control.Monad                as M
import           Control.Monad.Trans.Resource
import           Data.Conduit
import           Data.Conduit.List            as CL

main = do
  frames <- runResourceT $ videoFrameSource "v_Biking_g01_c01.avi" $$ CL.take 4
  M.zipWithM_ (\i frame -> plotFrame (show i ++ ".png") frame) [1 ..] frames
