import           CV.Array.LabeledArray
import           CV.IO.ImageIO
import           Data.Conduit          as C
import           Data.Conduit.List     as CL
import           System.Environment
import           Control.Monad.Trans.Resource
import           Data.Conduit.Binary          as CB

main = do
  (path:_) <- getArgs
  (img:_) <-
    runResourceT $
    CB.sourceFile path $$ readLabeledImagebinaryConduit =$=
    CL.map (\(LabeledArray _ arr) -> Image 8 arr) =$=
    CL.take 1
  plotImageRepa "test.png" img
