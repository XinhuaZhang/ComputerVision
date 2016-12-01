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
  trainLen <- fileLineCount trainPath
  let parallelParams =
        ParallelParams
        { numThread = 1
        , batchSize = 400
        }
      graySource path labelPath =
        imagePathSource path =$= grayImageConduit =$= CL.map grayImage2Array =$=
        mergeSource (labelSource labelPath) =$=
        CL.map (\(label, img) -> LabeledArray label img)
  graySource trainPath trainLabelPath $$
    rotateLabeledImageConduit parallelParams 256 0 =$=
    writeLabeledImageBinarySink (outputPath P.++ "test.bin") trainLen
  readLabeledImagebinarySource (outputPath P.++ "test.bin") $$
    CL.mapM_
      (\x ->
          let (LabeledArray label arr) = x
          in do print label
                print . P.head . R.toList $ arr)
