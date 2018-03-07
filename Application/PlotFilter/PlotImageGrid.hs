import           Application.PlotFilter.Grid
import           Codec.Picture
import           Control.Monad               as M
import           CV.IO.ImageIO
import           Data.List                   as L
import           System.Directory
import           System.Environment
import           System.FilePath
import           Text.Printf

main = do
  (folderPath:prefix:totalNumStr:colsStr:boderLenStr:_) <- getArgs
  images <-
    M.mapM
      (\i -> readImageRepa (printf "%s/%s_%03d.png" folderPath prefix i) True)
      [1 :: Int .. read totalNumStr]
  savePngImage
    (prefix L.++ ".png")
    (getGridImage (read colsStr) (read boderLenStr) (L.map imageContent images))
