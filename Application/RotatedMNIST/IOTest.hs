import           Application.RotatedMNIST.IO
import           Control.Monad                as M
import           Control.Monad.Trans.Resource
import           CV.Array.LabeledArray
import           CV.IO.ImageIO
import           Data.Conduit
import           Data.Conduit.Binary          as CB
import           Data.Conduit.List            as CL
import           Data.List                    as L
import           System.Environment
import Data.Array.Repa as R

main = do
  (path:_) <- getArgs
  xs <-
    runResourceT $
    sourceFile path $$ readLabeledImagebinaryConduit =$= CL.take 10
  M.zipWithM_
    (\(LabeledArray label img) i -> do
       print label
       plotImageRepa (show i L.++ ".png") . Image 8 $ img)
    xs
    [1 ..]
