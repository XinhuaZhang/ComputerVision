module CV.IO.ImageIO where

import           Control.Monad          (liftM)
import           Control.Monad.IO.Class (liftIO)
import           CV.Image
import           Data.Conduit           as C
import           Data.Conduit.List      as CL
import           Data.List              as L
import           Prelude                as P

data ImagePathType
  = GrayImagePathType
  | ColorImagePathType
  deriving (Show)

data ImagePath =
  ImagePath ImagePathType
            String
  deriving (Show)

readImagePathList :: FilePath -> IO [ImagePath]
readImagePathList filePath = do
  pathList <- liftM lines . readFile $ filePath
  let imageType
        | and . P.map (isSuffixOf ".pgm") $ pathList = GrayImagePathType
        | and . P.map (isSuffixOf ".ppm") $ pathList = ColorImagePathType
        | otherwise =
          error $ "Image type " P.++ imageSuffix P.++ " is not supported."
      dotIdx = elemIndices '.' . P.head $ pathList
      imageSuffix = snd . P.splitAt (P.last dotIdx) . P.head $ pathList
  return $ P.map (ImagePath imageType) pathList


imagePathSource :: FilePath -> Source IO ImagePath
imagePathSource filePath = do
  pathList <- liftIO $ readImagePathList filePath
  sourceList pathList

grayImageConduit :: Conduit ImagePath IO GrayImage
grayImageConduit = awaitForever readImg
  where
    readImg (ImagePath GrayImagePathType imagePath) =
      liftIO (readImage imagePath) >>= yield
    readImg _ = error "Image type is not GrayImage."

colorImageConduit :: Conduit ImagePath IO ColorImage
colorImageConduit = awaitForever readImg
  where
    readImg (ImagePath ColorImagePathType imagePath) =
      liftIO (readColorImage imagePath) >>= yield
    readImg _ = error "Image type is not ColorImage."
