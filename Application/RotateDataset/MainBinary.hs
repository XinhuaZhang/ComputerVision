module Main where

import           Application.RotateDataset.RotationRepa
import           Control.Monad.IO.Class                 (liftIO)
import           CV.Array.Image
import           CV.Array.LabeledArray
import           CV.IO.ImageIO
import           CV.Utility.RepaArrayUtility            as RAU
import           Data.Array.Repa                        as R
import           Data.Conduit                           as C
import           Data.Conduit.List                      as CL
import           Data.Image
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

main = do
  (trainPath:trainLabelPath:testPath:testLabelPath:outputPath:isColor:_) <-
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
      n = 256
      deg = 36
      graySource path labelPath =
        imagePathSource path =$= grayImageConduit =$= CL.map grayImage2Array =$=
        mergeSource (labelSource labelPath) =$=
        CL.map (\(label, img) -> LabeledArray label img)
      colorSource path labelPath =
        imagePathSource path =$= colorImageConduit =$= CL.map colorImage2Array =$=
        mergeSource (labelSource labelPath) =$=
        CL.map (\(label, img) -> LabeledArray label img)
  P.mapM_
    (\(x, y) ->
       createDirectoryIfMissing
         True
         (outputPath P.++ "/" P.++ x P.++ "/" P.++ y P.++ "/" P.++ str)) $
    (,) <$> ["Train", "Test"] <*> ["Original", "Rotated"]
  if read isColor :: Bool
    then do
      colorSource trainPath trainLabelPath $$
        writeLabeledImageSink
          (outputPath P.++ "/Train/Original/" P.++ str P.++ ".bin")
      colorSource trainPath trainLabelPath $$ rotateLabeledImageConduit n deg =$=
        writeLabeledImageSink
          (outputPath P.++ "/Train/Rotated/" P.++ str P.++ ".bin")
    else do
      graySource trainPath trainLabelPath $$
        writeLabeledImageSink
          (outputPath P.++ "/Train/Original/" P.++ str P.++ ".bin")
      graySource trainPath trainLabelPath $$ rotateLabeledImageConduit n deg =$=
        writeLabeledImageSink
          (outputPath P.++ "/Train/Rotated/" P.++ str P.++ ".bin")
