import           Application.RotatedMNIST.IO
import           Control.Monad                as M
import           Control.Monad.Trans.Resource
import           CV.Array.LabeledArray
import           CV.IO.ImageIO
import           Data.Conduit
import           Data.Conduit.List            as CL
import           Data.List                    as L
import           System.Environment
import           System.FilePath

main = do
  (folderPath:trainingPath:testingPath:_) <- getArgs
  xs <-
    runResourceT $ imageLabelSource (folderPath </> trainingPath) $$ CL.consume
  zs <-
    runResourceT $ imageLabelSource (folderPath </> testingPath) $$ CL.consume
  let (as, ys) = L.splitAt 10000 xs
  print . L.length $ xs
  print . L.length $ as
  print . L.length $ ys
  print . L.length $ zs
  runResourceT $
    CL.sourceList xs $$
    writeLabeledImageBinarySink (folderPath </> "Train_12000.dat") (L.length xs)
  runResourceT $
    CL.sourceList as $$
    writeLabeledImageBinarySink (folderPath </> "Train_10000.dat") (L.length as)
  runResourceT $
    CL.sourceList ys $$
    writeLabeledImageBinarySink
      (folderPath </> "Validate_2000.dat")
      (L.length ys)
  runResourceT $
    CL.sourceList zs $$
    writeLabeledImageBinarySink (folderPath </> "Test_50000.dat") (L.length zs)
