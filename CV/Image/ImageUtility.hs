module CV.Image.ImageUtility (rotateImage
                             ,padImage
                             ,resizeImages
                             ,resizeConduit) where

import           CV.Utility.Coordinates
import           CV.Utility.Parallel
import           Data.Conduit
import           Data.Conduit.List      as CL
import           Data.Image
import           Data.Maybe             as Maybe
import           Prelude                as P


rotateImage :: GrayImage -> Double -> GrayImage
rotateImage img rotateDeg = img1
  where (nx,ny) = dimensions img
        theta = deg2Rad rotateDeg
        (centerX,centerY) = ((fromIntegral nx) / 2,(fromIntegral ny) / 2)
        mat =
          P.map (\f -> f theta)
                [cos,sin,\x -> -(sin x),cos]
        img1 =
          makeImage nx
                    ny
                    (rotateImageOp img
                                   mat
                                   (centerX,centerY))

rotateImageOp :: GrayImage
              -> [Double]
              -> (Double,Double)
              -> Int
              -> Int
              -> (Pixel GrayImage)
rotateImageOp input mat (centerX,centerY) i j =
  ref' input
       (i' + centerX)
       (j' + centerY) -- ref' will check the boundary.
  where x = fromIntegral i - centerX
        y = fromIntegral j - centerY
        (i',j') =
          vecMatMult (x,y)
                     mat

vecMatMult
  :: (Double,Double) -> [Double] -> (Double,Double)
vecMatMult (x,y) (a:b:c:d:_) = (a * x + c * y,b * x + d * y)


padImage
  :: (Monoid a)
  => Int -> Int -> BoxedImage a -> BoxedImage a
padImage m n img
  | m <= nx && n <= ny = img
  | m > nx && n <= ny =
    makeImage m
              ny
              (\i j ->
                 if (i - (div (m - nx) 2)) < 0 ||
                    (i - (div (m - nx) 2)) > (nx - 1)
                    then mempty
                    else ref img (i - (div (m - nx) 2)) j)
  | m <= nx && n > ny =
    makeImage nx
              n
              (\i j ->
                 if (j - (div (n - ny) 2)) < 0 ||
                    (j - (div (n - ny) 2)) > (ny - 1)
                    then mempty
                    else ref img i (j - (div (n - ny) 2)))
  | otherwise =
    makeImage m
              n
              (\i j ->
                 if (i - (div (m - nx) 2)) < 0 ||
                    (i - (div (m - nx) 2)) > (nx - 1) ||
                    (div (n - ny) 2) < 0 || (j - (div (n - ny) 2)) > (ny - 1)
                    then mempty
                    else ref img
                             (i - (div (m - nx) 2))
                             (j - (div (n - ny) 2)))
  where (nx,ny) = dimensions img


resizeImages
  :: (Monoid a)
  => ParallelParams -> Int -> Int -> [BoxedImage a] -> [BoxedImage a]
resizeImages parallelParams m n = parMapChunk parallelParams rseq (resize m n)
  where
    resize
      :: (Monoid a)
      => Int -> Int -> BoxedImage a -> BoxedImage a
    resize m n img
      | m < nx && n < ny = crop (div (nx - m) 2) (div (ny - n) 2) m n img
      | m > nx && n < ny = padImage m n (crop 0 (div (ny - n) 2) nx n img)
      | m < nx && n > ny = padImage m n (crop (div (nx - m) 2) 0 m ny img)
      | otherwise = padImage m n img
      where
        (nx, ny) = dimensions img

resizeConduit
  :: (Monoid a)
  => ParallelParams -> Int -> Int -> Conduit (BoxedImage a) IO (BoxedImage a)
resizeConduit parallelParams resizeX resizeY = do
  batch <- CL.take (batchSize parallelParams)
  if P.length batch > 0
    then do
      let resizedImg = resizeImages parallelParams resizeX resizeY batch
      sourceList resizedImg
      resizeConduit parallelParams resizeX resizeY
    else return ()
