module Main where

import           Application.RotateDataset.Rotation
import           Control.Monad.IO.Class             (liftIO)
import           CV.Image
import           CV.Image.ImageUtility
import           CV.IO.ImageIO
import           CV.Utility.Parallel
import           Data.Conduit
import           Data.Conduit.List                  as CL
import           Data.Vector                        as V
import           Prelude                            as P
import           System.Directory
import           System.Environment

readLabelFile :: FilePath -> IO [Int]
readLabelFile filePath =
  do bs <- readFile filePath
     return . P.map (\x -> read x :: Int) . lines $! bs

labelSource :: FilePath -> Source IO Int
labelSource filePath =
  do labels <- liftIO . readLabelFile $ filePath
     sourceList labels

main =
  let parallelParams = ParallelParams {numThread = 8, batchSize = 800}
      (nx, ny) = (256, 256)
      deg = 36
  in do (trainPath:trainLabelPath:testPath:testLabelPath:outputPath:isColor:_) <-
          getArgs
        putStrLn $ "trainPath: " P.++ trainPath
        putStrLn $ "trainLabelPath: " P.++ trainLabelPath
        putStrLn $ "testPath: " P.++ testPath
        putStrLn $ "testLabelPath: " P.++ testLabelPath
        putStrLn $ "outputPath: " P.++ outputPath
        putStrLn $ "isColor: " P.++ isColor
        let str =
              if read isColor :: Bool
                then "Color"
                else "Gray"
        P.mapM_
          (\(x, y) ->
             createDirectoryIfMissing
               True
               (outputPath P.++ "/" P.++ x P.++ "/" P.++ y P.++ "/" P.++ str)) $
          (,) <$> ["Train", "Test"] <*> ["Original", "Rotated"]
        if read isColor :: Bool
          then do
            imagePathSource trainPath $$ colorImageConduit =$=
              resizeConduit parallelParams nx ny =$=
              mergeSource (labelSource trainLabelPath) =$=
              CL.map (\(label, img) -> LabelColorImage label img) =$=
              outputLabelImageSink (outputPath P.++ "/Train/Original/" P.++ str)
            imagePathSource trainPath $$ colorImageConduit =$=
              resizeConduit parallelParams nx ny =$=
              mergeSource (labelSource trainLabelPath) =$=
              CL.map (\(label, img) -> LabelColorImage label img) =$=
              rotateLabelImagesConduit parallelParams deg =$
              outputLabelImageSink (outputPath P.++ "/Train/Rotated/" P.++ str)
            imagePathSource testPath $$ colorImageConduit =$=
              resizeConduit parallelParams nx ny =$=
              mergeSource (labelSource testLabelPath) =$=
              CL.map (\(label, img) -> LabelColorImage label img) =$=
              outputLabelImageSink (outputPath P.++ "/Test/Original/" P.++ str)
            imagePathSource testPath $$ colorImageConduit =$=
              resizeConduit parallelParams nx ny =$=
              mergeSource (labelSource testLabelPath) =$=
              CL.map (\(label, img) -> LabelColorImage label img) =$=
              rotateLabelImagesConduit parallelParams deg =$
              outputLabelImageSink (outputPath P.++ "/Test/Rotated/" P.++ str)
          else do
            imagePathSource trainPath $$ grayImageConduit =$=
              resizeConduit parallelParams nx ny =$=
              mergeSource (labelSource trainLabelPath) =$=
              CL.map (\(label, img) -> LabelGrayImage label img) =$=
              outputLabelImageSink (outputPath P.++ "/Train/Original/" P.++ str)
            imagePathSource trainPath $$ grayImageConduit =$=
              resizeConduit parallelParams nx ny =$=
              mergeSource (labelSource trainLabelPath) =$=
              CL.map (\(label, img) -> LabelGrayImage label img) =$=
              rotateLabelImagesConduit parallelParams deg =$
              outputLabelImageSink (outputPath P.++ "/Train/Rotated/" P.++ str)
            imagePathSource testPath $$ grayImageConduit =$=
              resizeConduit parallelParams nx ny =$=
              mergeSource (labelSource testLabelPath) =$=
              CL.map (\(label, img) -> LabelGrayImage label img) =$=
              outputLabelImageSink (outputPath P.++ "/Test/Original/" P.++ str)
            imagePathSource testPath $$ grayImageConduit =$=
              resizeConduit parallelParams nx ny =$=
              mergeSource (labelSource testLabelPath) =$=
              CL.map (\(label, img) -> LabelGrayImage label img) =$=
              rotateLabelImagesConduit parallelParams deg =$
              outputLabelImageSink (outputPath P.++ "/Test/Rotated/" P.++ str)
