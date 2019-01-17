import           Application.STL10.IO
import           Control.Monad                as M
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           CV.Array.Image
import           Data.Array.Repa              as R
import           Data.Conduit
import           Data.Conduit.Binary          as CB
import           Data.Conduit.List            as CL
import           Data.List                    as L
import           System.Directory
import           System.Environment
import           System.FilePath
import           System.IO
import           System.Random

writeImageSink :: FilePath
               -> Int
               -> Handle
               -> Sink (Array U DIM3 Double) (ResourceT IO) ()
writeImageSink folderPath n h = do
  x <- await
  case x of
    Nothing -> return ()
    Just y -> do
      -- liftIO $ plotImage (folderPath </> ((show n) L.++ ".png")) y
      liftIO . hPutStrLn h $ (folderPath </> ((show n) L.++ ".png"))
      writeImageSink folderPath (n + 1) h

main = do
  (imagePath:labelPath:labelFlagStr:folderName:_) <- getArgs
  createDirectoryIfMissing True folderName
  h <- openFile (folderName </> "image.txt") WriteMode
  runResourceT $
    CB.sourceFile imagePath $$ readSTL10ImageConduit =$=
    writeImageSink folderName 1 h
  hClose h
  when
    (read labelFlagStr :: Bool)
    (do labels <-
          runResourceT $
          CB.sourceFile labelPath $$ readSTL10LabelConduit =$= CL.consume
        writeFile (folderName </> "label.txt") . L.unlines . L.map show $ labels)
