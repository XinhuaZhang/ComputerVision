import           Application.CaffeData.Caffe
import           Control.Monad                as M
import           Control.Monad.Trans.Resource
import           CV.Array.LabeledArray
import           Data.Conduit
import           Data.Conduit.Binary          as CB
import           Data.Conduit.List            as CL
import           System.Environment
import           System.FilePath

main = do
  (filePath:_) <- getArgs
  runResourceT $
    CB.sourceFile filePath $$ readLabeledImagebinaryConduit =$=
    saveDataSink (takeBaseName filePath) 500
