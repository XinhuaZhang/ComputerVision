import           Application.PlotFilter.Grid
import           Codec.Picture
import           Control.Monad               as M
import           CV.IO.ImageIO
import           Data.List                   as L
import           System.Directory
import           System.Environment
import           System.FilePath
import           Text.Printf

{-# INLINE splitList #-}

splitList :: Int -> [a] -> [[a]]
splitList _ [] = []
splitList n xs =
  let (as, bs) = L.splitAt n xs
  in as : splitList n bs
  

main = do
  (folderPath:prefix:totalNumStr:colsStr:boderLenStr:_) <- getArgs
  images <-
    M.mapM
      (\i -> readImageRepa (printf "%s/%s_%d.png" folderPath prefix i) True)
      [1 :: Int .. read totalNumStr]
  let xs = splitList (read colsStr) images
      newCols = L.length xs
      ys = L.concat xs
  print newCols
  savePngImage
    (prefix L.++ ".png")
    (getGridImage (newCols) (read boderLenStr) (L.map imageContent ys))
