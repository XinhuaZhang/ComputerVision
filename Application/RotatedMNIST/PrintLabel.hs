import           Application.RotatedMNIST.ArgsParser as AP
import           Control.Arrow
import           Control.Monad                       as M
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           CV.Array.LabeledArray
import           CV.IO.ImageIO
import           CV.Utility.Parallel                 as Par
import           Data.Array.Repa                     as R
import           Data.Binary
import           Data.Conduit
import           Data.Conduit.Binary                 as CB
import           Data.Conduit.List                   as CL
import           Data.List                           as L
import           Data.Vector                         as V
import           Data.Vector.Unboxed                 as VU
import           System.Environment

main = do
  (inputFile:_) <- getArgs
  xs <-
    runResourceT $
    CB.sourceFile inputFile $$ readLabeledImagebinaryConduit =$=
    CL.map (\(LabeledArray l _) -> l) =$=
    CL.take 100
  print xs
