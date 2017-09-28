import           Application.ShiftableTest.FastWaveletTransform
import           CV.IO.ImageIO
import           Data.Array.Repa                            as R
import           Data.Image
import           System.Environment
import           System.FilePath

main = do
  (filePath:numStr:_) <- getArgs
  repaImg <- readImageRepa filePath False
  let (Z :. _ :. rows :. cols) = extent . imageContent $ repaImg
      img =
        makeImage
          rows
          cols
          (\i j -> (imageContent repaImg) R.! (Z :. (0 :: Int) :. i :. j)) :: GrayImage
      results =
        fastWaveletTransform (read numStr :: Int) daubechies4H0 daubechies4H1 img :: [[GrayImage]]
  plotFastWaveletTransform (dropExtension filePath) results
