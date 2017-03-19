import           Application.HandWriting.IO
import           Control.Monad.Trans.Resource
import           Data.Conduit
import           Data.Conduit.List            as CL
import           System.Environment

main = do
  (path:_) <- getArgs
  num <-
    runResourceT $
    hwdbSource path $$ offlineCharacterConduit =$=
    CL.fold (\b _ -> b + 1) (0 :: Int)
  print num
