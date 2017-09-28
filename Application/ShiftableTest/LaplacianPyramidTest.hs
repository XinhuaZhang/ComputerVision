import           Application.ShiftableTest.LaplacianPyramid
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
      results = laplacianPyramid (read numStr :: Int) img
  plotLaplacianPyramid (dropExtension filePath) results
