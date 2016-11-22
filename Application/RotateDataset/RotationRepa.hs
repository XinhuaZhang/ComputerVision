{-# LANGUAGE FlexibleContexts #-}

module Application.RotateDataset.RotationRepa where

import           Control.Monad               as M
import           Control.Monad.IO.Class
import           CV.Array.Image
import           CV.Array.LabeledArray
import           CV.Utility.Coordinates
import           CV.Utility.Parallel
import           CV.Utility.RepaArrayUtility
import           Data.Array.Repa             as R
import           Data.Binary
import           Data.Conduit
import           Data.Conduit.List           as CL
import           Data.List                   as L
import           Data.Vector                 as V
import           Data.Vector.Unboxed         as VU
import           Prelude                     as P
import           System.IO
import CV.Utility.Parallel
import Data.ByteString.Lazy as BL
import qualified Data.Image as IM

-- First pading image to be a square image then rotating it
recaleAndRotate2DImageS
  :: (R.Source s Double)
  => Int -> [Double] -> Array s DIM2 Double -> [Array U DIM2 Double]
recaleAndRotate2DImageS n degs arr =
  parMap rseq
    (\deg ->
       computeS $
       fromFunction
         (Z :. n :. n)
         (\(Z :. j :. i) ->
            let (j', i') =
                  rotatePixel
                    (VU.fromListN 4 $
                     P.map
                       (\f -> f (deg2Rad deg))
                       [cos, sin, \x -> -(sin x), cos])
                    (center, center)
                    (fromIntegral j, fromIntegral i)
            in if inRange j' && inRange i'
                 then bicubicInterpolation
                        ds
                        ( (j' - boundaryWith) * ratio
                        , (i' - boundaryWith) * ratio)
                 else 0))
    degs
  where
    (Z :. ny :. nx) = extent arr
    m = max ny nx
    theta = atan (fromIntegral nx / fromIntegral ny)
    xx = max (cos theta) (sin theta)
    innerSize = floor (fromIntegral n * xx)
    boundaryWith = fromIntegral $ div (n - innerSize) 2
    paddedImg = pad [m, m] arr
    ds = computeDerivativeS (computeUnboxedS paddedImg)
    center = fromIntegral n / 2
    ratio = fromIntegral m / (fromIntegral innerSize - 1)
    inRange :: Double -> Bool
    inRange x =
      x >= boundaryWith && x <= (boundaryWith + fromIntegral innerSize - 1)

rotatePixel :: VU.Vector Double
            -> (Double, Double)
            -> (Double, Double)
            -> (Double, Double)
rotatePixel mat (centerY, centerX) (y, x) = (y3, x3)
  where
    x1 = x - centerX
    y1 = y - centerY
    (y2, x2) = vecMatMult (y1, x1) mat
    x3 = x2 + centerX
    y3 = y2 + centerY

vecMatMult :: (Double, Double) -> VU.Vector Double -> (Double, Double)
vecMatMult (x, y) vec = (a * x + c * y, b * x + d * y)
  where
    a = vec VU.! 0
    b = vec VU.! 1
    c = vec VU.! 2
    d = vec VU.! 3

rotateLabeledImageConduit
  :: Int
  -> Double
  -> Conduit (LabeledArray DIM3 Double) IO (LabeledArray DIM3 Double)
rotateLabeledImageConduit n deg =
  awaitForever
    (\(LabeledArray label arr) ->
       let (Z :. nf :. _ny :. _nx) = extent arr
       in sourceList .
          L.map
            (LabeledArray label . 
             fromUnboxed (Z :. nf :. n :. n) . VU.concat . L.map R.toUnboxed) .
          L.transpose .
          L.map
            (\i ->
               recaleAndRotate2DImageS n degs $
               R.slice arr (Z :. i :. All :. All)) $
          [0 .. nf - 1])
  where
    len = round (360 / deg)
    degs = L.map (* deg) [0 .. fromIntegral len - 1]


writeLabeledImageBinarySink :: FilePath
                            -> Int
                            -> Sink (LabeledArray DIM3 Double) IO ()
writeLabeledImageBinarySink filePath len = do
  h <- liftIO $ openBinaryFile filePath WriteMode
  liftIO $ BL.hPut h (encode len)
  CL.foldMapM
    (\(LabeledArray label arr) ->
       BL.hPut
         h
         (encode .
          LabeledArray label .
          computeS .
          R.map (\x -> round x :: Word) .
          normalizeImage (fromIntegral (maxBound :: Word)) $
          arr))
  liftIO $ hClose h

writeLabeledImageSink :: FilePath
                      -> Sink (LabeledArray DIM3 Double) IO ()
writeLabeledImageSink filePath = do
  hImg <- liftIO $ openFile (filePath P.++ "/ImageList.txt") WriteMode
  hLabel <- liftIO $ openFile (filePath P.++ "/label.txt") WriteMode
  CL.foldM
    (\index (LabeledArray label arr') ->
       let (Z :. nf :. ny :. nx) = extent arr'
           arr =
             computeUnboxedS . normalizeImage (fromIntegral (maxBound :: Word)) $
             arr'
       in case nf of
            1 -> do
              hPutStrLn hImg (filePath P.++ "/" P.++ (show index) P.++ ".pgm")
              hPutStrLn hLabel (show label)
              IM.writeImage (filePath P.++ "/" P.++ show index P.++ ".pgm") $
                (IM.makeImage ny nx (\j i -> arr R.! (Z :. 0 :. ny :. nx)) :: IM.GrayImage)
              return $ index + 1
            3 -> do
              hPutStrLn hImg (filePath P.++ "/" P.++ (show index) P.++ ".pgm")
              hPutStrLn hLabel (show label)
              IM.writeImage (filePath P.++ "/" P.++ show index P.++ ".pgm") $
                (let r =
                       IM.makeImage ny nx (\j i -> arr R.! (Z :. 0 :. ny :. nx)) :: IM.GrayImage
                     g =
                       IM.makeImage ny nx (\j i -> arr R.! (Z :. 1 :. ny :. nx)) :: IM.GrayImage
                     b =
                       IM.makeImage ny nx (\j i -> arr R.! (Z :. 2 :. ny :. nx)) :: IM.GrayImage
                 in IM.rgbToColorImage (r, g, b))
              return $ index + 1
            _ -> error "Image channels are neither 1 nor 3.")
    0
  liftIO $ hClose hImg
  liftIO $ hClose hLabel
