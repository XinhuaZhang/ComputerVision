import           Application.CaffeData.HDF5
import           Control.Monad                as M
import           Control.Monad.Trans.Resource
import           CV.Array.LabeledArray
import           CV.Utility.Parallel
import           Data.Conduit
import           Data.Conduit.Binary          as CB
import           Data.Conduit.List            as CL
import           System.Environment
import           System.FilePath

main = do
  (filePath:_) <- getArgs
  runResourceT $
    CB.sourceFile filePath $$ readLabeledImagebinaryConduit =$= CL.map ((:) []) =$=
    hdf5Sink (ParallelParams 1 2000) (takeBaseName filePath)
