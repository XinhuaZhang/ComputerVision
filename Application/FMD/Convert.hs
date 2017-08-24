import           Application.FMD.PathGenerator
import           Control.Monad.Trans.Resource
import           CV.Array.LabeledArray
import           CV.IO.ImageIO
import           CV.Utility.Parallel
import           Data.Conduit                  as C
import           Data.Conduit.List             as CL
import           Data.List                     as L
import           System.Environment

main = do
  (folderPath:sizeStr:colorFlagStr:_) <- getArgs
  let parallelParams =
        ParallelParams
        { numThread = 4
        , batchSize = 100
        }
      (trainLabels, trainPaths) = L.unzip $ halfPathGenerator folderPath True
      (testLabels, testPaths) = L.unzip $ halfPathGenerator folderPath False
  if read colorFlagStr :: Bool
    then do
      runResourceT $
        sourceList trainPaths $$ readImageConduit True =$=
        mergeSource (sourceList trainLabels) =$=
        CL.map (\(label, img) -> LabeledArray (round label) (imageContent img)) =$=
        resizeLabeledArrayConduit parallelParams (read sizeStr :: Int) =$=
        writeLabeledImageBinarySink (sizeStr L.++ "_Color_Train.dat") 500
      runResourceT $
        sourceList testPaths $$ readImageConduit True =$=
        mergeSource (sourceList testLabels) =$=
        CL.map (\(label, img) -> LabeledArray (round label) (imageContent img)) =$=
        resizeLabeledArrayConduit parallelParams (read sizeStr :: Int) =$=
        writeLabeledImageBinarySink (sizeStr L.++ "_Color_Test.dat") 500
    else do
      runResourceT $
        sourceList trainPaths $$ readImageConduit False =$=
        mergeSource (sourceList trainLabels) =$=
        CL.map (\(label, img) -> LabeledArray (round label) (imageContent img)) =$=
        resizeLabeledArrayConduit parallelParams (read sizeStr :: Int) =$=
        writeLabeledImageBinarySink (sizeStr L.++ "_Gray_Train.dat") 500
      runResourceT $
        sourceList testPaths $$ readImageConduit False =$=
        mergeSource (sourceList testLabels) =$=
        CL.map (\(label, img) -> LabeledArray (round label) (imageContent img)) =$=
        resizeLabeledArrayConduit parallelParams (read sizeStr :: Int) =$=
        writeLabeledImageBinarySink (sizeStr L.++ "_Gray_Test.dat") 500
