import           CV.Array.Image
import           CV.IO.ImageIO
import           Data.List          as L
import           System.Environment
import           System.FilePath
import Data.Array.Repa as R

main = do
  (filePath:_) <- getArgs
  img <- readImageRepa filePath False
  let x = 0
      y = -30
  plotImage (dropExtension filePath L.++ "_" L.++ show (x, y) <.> "png") .
    shiftImage x y 0 . imageContent $
    img
  -- let arr =
  --       shiftGrayImage x y 0 . R.slice (imageContent img) $
  --       (Z :. (0 :: Int) :. All :. All)
  --     (Z :. rows :. cols) = extent arr
  -- plotImage (dropExtension filePath L.++ "_" L.++ show (x, y) <.> "png") .
  --   fromUnboxed (Z :. (1 :: Int) :. rows :. cols) . toUnboxed $
  --   arr
