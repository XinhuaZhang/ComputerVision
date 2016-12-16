{-# LANGUAGE BangPatterns #-}
module CV.IO.ImageIO where

import           Codec.Picture
import           Control.Monad.IO.Class (liftIO)
import           Data.Array.Repa        as R
import           Data.Conduit           as C
import           Data.Conduit.List      as CL
import           GHC.Float

readImagePathList :: FilePath -> IO [String]
readImagePathList = fmap lines . readFile

imagePathSource :: FilePath -> C.Source IO FilePath
imagePathSource filePath = do
  pathList <- liftIO $ readImagePathList filePath
  sourceList pathList

readImageConduit :: Conduit FilePath IO (Array D DIM3 Double)
readImageConduit =
  awaitForever
    (\filePath -> do
       buffer <- liftIO $ readImage filePath
       case buffer of
         Left msg -> error msg
         Right dImg ->
           let arr =
                 case dImg of
                   ImageY8 img ->
                     fromFunction
                       (Z :. (1 :: Int) :. imageHeight img :. imageWidth img)
                       (\(Z :. _ :. j :. i) ->
                           fromIntegral $ pixelAt img j i :: Double)
                   ImageY16 img ->
                     fromFunction
                       (Z :. (1 :: Int) :. imageHeight img :. imageWidth img)
                       (\(Z :. _ :. j :. i) ->
                           fromIntegral $ pixelAt img j i :: Double)
                   ImageYF img ->
                     fromFunction
                       (Z :. (1 :: Int) :. imageHeight img :. imageWidth img)
                       (\(Z :. _ :. j :. i) ->
                           float2Double $ pixelAt img j i :: Double)
                   ImageRGB8 img ->
                     fromFunction
                       (Z :. (3 :: Int) :. imageHeight img :. imageWidth img)
                       (\(Z :. k :. j :. i) ->
                           let !(PixelRGB8 r g b) = pixelAt img j i
                           in case k of
                                0 -> fromIntegral r
                                1 -> fromIntegral g
                                2 -> fromIntegral b
                                _ -> error "readImageConduit: dimension error.")
                   ImageRGB16 img ->
                     fromFunction
                       (Z :. (3 :: Int) :. imageHeight img :. imageWidth img)
                       (\(Z :. k :. j :. i) ->
                           let !(PixelRGB16 r g b) = pixelAt img j i
                           in case k of
                                0 -> fromIntegral r
                                1 -> fromIntegral g
                                2 -> fromIntegral b
                                _ -> error "readImageConduit: dimension error.")
                   ImageRGBF img ->
                     fromFunction
                       (Z :. (3 :: Int) :. imageHeight img :. imageWidth img)
                       (\(Z :. k :. j :. i) ->
                           let !(PixelRGBF r g b) = pixelAt img j i
                           in case k of
                                0 -> float2Double r
                                1 -> float2Double g
                                2 -> float2Double b
                                _ -> error "readImageConduit: dimension error.")
                   _ -> error "readImageConduit: image type is not supported"
           in yield arr)
