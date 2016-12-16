module Main where

import           Application.RotateDataset.RotationRepa
import           Control.Monad.IO.Class                 (liftIO)
import           CV.Array.Image
import           CV.Array.LabeledArray
import           CV.IO.ImageIO
import           CV.Utility.Parallel
import           Data.Array.Repa                        as R
import           Data.Conduit                           as C
import           Data.Conduit.List                      as CL
import           Prelude                                as P
import           System.Directory
import           System.Environment

readLabelFile :: FilePath -> IO [Int]
readLabelFile filePath =
  do bs <- readFile filePath
     return . P.map (\x -> read x :: Int) . lines $! bs

labelSource :: FilePath -> C.Source IO Int
labelSource filePath =
  do labels <- liftIO . readLabelFile $ filePath
     sourceList labels

fileLineCount :: FilePath -> IO Int
fileLineCount filePath = do
  xs <- readFile filePath
  return . P.length . lines $ xs

main = do
  (trainPath:trainLabelPath:testPath:testLabelPath:outputPath:isColor:_) <- getArgs
  putStrLn $ "trainPath: " P.++ trainPath
  putStrLn $ "trainLabelPath: " P.++ trainLabelPath
  putStrLn $ "testPath: " P.++ testPath
  putStrLn $ "testLabelPath: " P.++ testLabelPath
  putStrLn $ "outputPath: " P.++ outputPath
  putStrLn $ "isColor: " P.++ isColor
  let parallelParams =
        ParallelParams
        { numThread = 14
        , batchSize = 1400
        }
      isColorFlag = read isColor :: Bool
      str =
        if isColorFlag
          then "Color"
          else "Gray"
      n = 256
      deg = 90
      rotationLen = round (360 / deg)
      imageSource path labelPath =
        imagePathSource path =$= readImageConduit isColorFlag =$=
        mergeSource (labelSource labelPath) =$=
        CL.map (\(label, img) -> LabeledArray label $ computeUnboxedS img)
  trainLen <- fileLineCount trainPath
  testLen <- fileLineCount testPath
  P.mapM_
    (\(x, y) ->
        createDirectoryIfMissing
          True
          (outputPath P.++ "/" P.++ x P.++ "/" P.++ y P.++ "/" P.++ str)) $
    (,) <$> ["Train", "Test"] <*> ["Original", "Rotated"]
  imageSource trainPath trainLabelPath $$
    rescaleRotateLabeledImageConduit parallelParams n 0 =$=
    writeLabeledImageBinarySink
      (outputPath P.++ "/Train/Original/" P.++ str P.++ ".bin")
      trainLen
  imageSource trainPath trainLabelPath $$
    rescaleRotateLabeledImageConduit parallelParams n deg =$=
    writeLabeledImageBinarySink
      (outputPath P.++ "/Train/Rotated/" P.++ str P.++ ".bin")
      (trainLen * rotationLen)
  imageSource testPath testLabelPath $$
    rescaleRotateLabeledImageConduit parallelParams n 0 =$=
    writeLabeledImageBinarySink
      (outputPath P.++ "/Test/Original/" P.++ str P.++ ".bin")
      testLen
  imageSource testPath testLabelPath $$
    rescaleRotateLabeledImageConduit parallelParams n deg =$=
    writeLabeledImageBinarySink
      (outputPath P.++ "/Test/Rotated/" P.++ str P.++ ".bin")
      (testLen * rotationLen)
