import           Control.Monad      as M
import           CV.Array.Image
import           CV.IO.ImageIO
import           Data.Array.Repa    as R
import           Data.List          as L
import           System.Environment
import           System.FilePath

main = do
  (filePath:_) <- getArgs
  repaImg <- readImageRepa filePath False
  let (Z :. _ :. rows :. cols) = extent . imageContent $ repaImg
      pImage =
        cartesian2logpolarImage
          512
          512
          (fromIntegral $ div rows 2, fromIntegral $ div cols 2)
          (fromIntegral $ min (div rows 2) (div cols 2)) .
        CartesianImage (0, 255) . imageContent $
        repaImg
      cImage = logpolar2CartesianImage 128 128 (64, 64) pImage
  plotImage (dropExtension filePath L.++ "_logpolarImage.png") .
    getLogpolarImage $
    pImage
  plotImage (dropExtension filePath L.++ "_cartesianImage.png") .
    getCartesianImage $
    cImage
