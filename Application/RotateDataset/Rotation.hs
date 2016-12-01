module  Application.RotateDataset.Rotation where

import           Control.Monad          as M
import           Control.Monad.IO.Class
import qualified Control.Monad.Parallel as MP
import           CV.Image
import           CV.Image.ImageUtility
import           CV.Utility.Parallel
import           Data.Conduit
import qualified Data.Conduit.List      as CL
import           Data.List              as L
import           Data.Vector            as V
import           Prelude                as P
import           System.IO

data LabelImage
  = LabelGrayImage !Int
                   GrayImage
  | LabelColorImage !Int
                    ColorImage
  deriving ((Show))

rotateLabelImages :: ParallelParams -> [LabelImage] -> Double -> [LabelImage]
rotateLabelImages params xs deg =
  P.concat . parMapChunk params rseq rotateLabelImage $! xs
  where len = round (360 / deg)
        rotateLabelImage
          :: LabelImage -> [LabelImage]
        rotateLabelImage (LabelGrayImage label img) =
          V.toList . V.map (LabelGrayImage label) $
          rotateImageS
            img
            (V.generate len
                        (\i -> (P.fromIntegral i) * deg))
        rotateLabelImage (LabelColorImage label img) =
          P.map ((LabelColorImage label) .
                 rgbToColorImage . (\(a:b:c:_) -> (a,b,c))) .
          L.transpose .
          P.map (\img' ->
                   V.toList $
                   rotateImageS
                     img'
                     (V.generate len
                                 (\i -> (P.fromIntegral i) * deg))) $!
          [r,g,b]
          where (r,g,b) = colorImageToRGB img

rotateLabelImagesConduit
  :: ParallelParams -> Double -> Conduit LabelImage IO LabelImage
rotateLabelImagesConduit parallelParams deg =
  do xs <- CL.take $ batchSize parallelParams
     if P.length xs > 0
        then do CL.sourceList $ rotateLabelImages parallelParams xs deg
                rotateLabelImagesConduit parallelParams deg
        else return ()



outputLabelImage :: FilePath -> [LabelImage] -> IO ()
outputLabelImage filePath xs =
  do hImg <- openFile (filePath P.++ "/ImageList.txt") WriteMode
     hLabel <- openFile (filePath P.++ "/label.txt") WriteMode
     M.zipWithM_ (write hImg hLabel)
                 xs
                 [1 ..]
     hClose hImg
     hClose hLabel
  where write
          :: Handle -> Handle -> LabelImage -> Int -> IO ()
        write handleImage handleLabel (LabelGrayImage label img) index =
          do hPutStrLn handleImage (filePath P.++ "/" P.++ show index P.++ ".pgm")
             hPutStrLn handleLabel (show label)
             writeImage (filePath P.++ "/" P.++ show index P.++ ".pgm")
                        img
        write handleImage handleLabel (LabelColorImage label img) index =
          do hPutStrLn handleImage (filePath P.++ "/" P.++ show index P.++ ".ppm")
             hPutStrLn handleLabel (show label)
             writeImage (filePath P.++ "/" P.++ show index P.++ ".ppm")
                        img

outputLabelImageSink :: FilePath -> Sink LabelImage IO ()
outputLabelImageSink filePath =
  do xs <- CL.consume
     hImg <- liftIO $ openFile (filePath P.++ "/ImageList.txt") WriteMode
     hLabel <- liftIO $ openFile (filePath P.++ "/label.txt") WriteMode
     liftIO $
       MP.sequence_ $
       P.zipWith (write hImg hLabel)
                 xs
                 [1 ..]
     liftIO $ hClose hImg
     liftIO $ hClose hLabel
  where write
          :: Handle -> Handle -> LabelImage -> Int -> IO ()
        write handleImage handleLabel (LabelGrayImage label img) index =
          do hPutStrLn handleImage
                       (filePath P.++ "/" P.++ show index P.++ ".pgm")
             hPutStrLn handleLabel (show label)
             writeImage (filePath P.++ "/" P.++ show index P.++ ".pgm")
                        img
        write handleImage handleLabel (LabelColorImage label img) index =
          do hPutStrLn handleImage
                       (filePath P.++ "/" P.++ show index P.++ ".ppm")
             hPutStrLn handleLabel (show label)
             writeImage (filePath P.++ "/" P.++ show index P.++ ".ppm")
                        img

-- outputLabelImageSink :: FilePath -> Sink LabelImage IO ()
-- outputLabelImageSink filePath =
--   do hImg <- liftIO $ openFile (filePath P.++ "/ImageList.txt") WriteMode
--      hLabel <- liftIO $ openFile (filePath P.++ "/label.txt") WriteMode
--      CL.foldM (write hImg hLabel) 0
--      liftIO $ hClose hImg
--      liftIO $ hClose hLabel
--   where write
--           :: Handle -> Handle -> Int -> LabelImage -> IO Int
--         write handleImage handleLabel index (LabelGrayImage label img) =
--           do hPutStrLn handleImage (filePath P.++ "/" P.++ show index P.++ ".pgm")
--              hPutStrLn handleLabel (show label)
--              writeImage (filePath P.++ "/" P.++ show index P.++ ".pgm")
--                         img
--              return (index + 1)
--         write handleImage handleLabel index (LabelColorImage label img) =
--           do hPutStrLn handleImage (filePath P.++ "/" P.++ show index P.++ ".ppm")
--              hPutStrLn handleLabel (show label)
--              writeImage (filePath P.++ "/" P.++ show index P.++ ".ppm")
--                         img
--              return (index + 1)

