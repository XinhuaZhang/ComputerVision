import           Application.RotateDataset.RotationRepa
import           Control.Monad                          as M
import           CV.Array.Image
import           CV.Array.LabeledArray
import           Data.Array.Repa                        as R
import           Data.List                              as L
import           System.Environment

main =
  do (inputPath:_) <- getArgs
     let numTake = 50
         numDrop = 40
     images <- readLabeledImageBinary inputPath numTake
     let imgs = L.map (\(LabeledArray _ img) -> img) .  L.drop numDrop $ images
     M.zipWithM_ (\img i -> plotImage (show i L.++ ".png") img)
                 imgs
                 [1 ..]
     print . L.map sumAllS $ imgs
