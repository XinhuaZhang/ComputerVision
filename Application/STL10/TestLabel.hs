import           Application.STL10.IO
import           Control.Monad                as M
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           Data.Conduit
import           Data.Conduit.Binary          as CB
import           Data.Conduit.List            as CL
import           Data.List                    as L
import           System.Directory
import           System.Environment
import           System.FilePath
import           System.Random


main = do
  (labelPath:_) <- getArgs
  xs <-
    runResourceT $
    CB.sourceFile labelPath $$ readSTL10LabelConduit =$= CL.take 10
  print xs
