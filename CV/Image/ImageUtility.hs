{-# LANGUAGE BangPatterns #-}
module CV.Image.ImageUtility
  (rotateImage
  ,rotateImageS
  ,rotateImageP
  ,padImage
  ,resizeImages
  ,resizeConduit)
  where

import           Control.Monad                as M
import           CV.Utility.Coordinates
import           CV.Utility.Parallel
import           CV.Utility.RepaArrayUtility
import           Data.Array.Repa              as R
import           Data.Conduit
import           Data.Conduit.List            as CL
import           Data.Image                   as IM
import           Data.Vector                  as V
import           Data.Vector.Unboxed          as VU
import           Prelude                      as P

padImage
  :: (Monoid a)
  => Int -> Int -> BoxedImage a -> BoxedImage a
padImage m n img
  | m <= nx && n <= ny = img
  | m > nx && n <= ny =
    makeImage m
              ny
              (\i j ->
                 if i - div (m - nx) 2 < 0 || i - div (m - nx) 2 > (nx - 1)
                    then mempty
                    else ref img (i - div (m - nx) 2) j)
  | m <= nx && n > ny =
    makeImage nx
              n
              (\i j ->
                 if j - div (n - ny) 2 < 0 || j - div (n - ny) 2 > (ny - 1)
                    then mempty
                    else ref img i (j - div (n - ny) 2))
  | otherwise =
    makeImage m
              n
              (\i j ->
                 if i - div (m - nx) 2 < 0 ||
                    i - div (m - nx) 2 > (nx - 1) ||
                    j - div (n - ny) 2 < 0 || j - div (n - ny) 2 > (ny - 1)
                    then mempty
                    else ref img
                             (i - div (m - nx) 2)
                             (j - div (n - ny) 2))
  where (nx,ny) = dimensions img

resizeImages :: (Monoid a)
             => ParallelParams
             -> Int
             -> Int
             -> [BoxedImage a]
             -> [BoxedImage a]
resizeImages parallelParams m n = parMapChunk parallelParams rseq resize
  where resize :: (Monoid a)
               => BoxedImage a -> BoxedImage a
        resize img
          | m < nx && n < ny =
            IM.crop (div (nx - m) 2)
                    (div (ny - n) 2)
                    m
                    n
                    img
          | m > nx && n < ny =
            padImage m n (IM.crop 0 (div (ny - n) 2) nx n img)
          | m < nx && n > ny =
            padImage m n (IM.crop (div (nx - m) 2) 0 m ny img)
          | otherwise = padImage m n img
          where (nx,ny) = dimensions img

resizeConduit
  :: (Monoid a)
  => ParallelParams -> Int -> Int -> Conduit (BoxedImage a) IO (BoxedImage a)
resizeConduit parallelParams resizeX resizeY =
  do batch <- CL.take (batchSize parallelParams)
     unless (P.null batch)
            (do let resizedImg =
                      resizeImages parallelParams resizeX resizeY batch
                sourceList resizedImg
                resizeConduit parallelParams resizeX resizeY)

vecMatMult
  :: (Double,Double) -> VU.Vector Double -> (Double,Double)
vecMatMult (x,y) vec = (a * x + c * y,b * x + d * y)
  where a = vec VU.! 0
        b = vec VU.! 1
        c = vec VU.! 2
        d = vec VU.! 3

-- Only one rotation
rotateImage :: GrayImage -> Double -> GrayImage
rotateImage img rotateDeg =
  makeImage ny
            nx
            (\j i ->
               bicubicInterpolation ds matrixA $
               rotatePixel mat
                           (centerY,centerX)
                           (fromIntegral j,fromIntegral i))
  where (ny,nx) = dimensions img
        theta = deg2Rad rotateDeg
        (centerX,centerY) = (fromIntegral nx / 2,fromIntegral ny / 2)
        mat =
          VU.fromListN 4 $
          P.map (\f -> f theta)
                [cos,sin,\x -> -(sin x),cos]
        !ds =
          computeDerivativeS . fromListUnboxed (Z :. ny :. nx) . pixelList $
          img
        matrixA =
          V.fromListN 16 . P.map (VU.fromListN 16) $
          [[1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
          ,[0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0]
          ,[-3,3,0,0,-2,-1,0,0,0,0,0,0,0,0,0,0]
          ,[2,-2,0,0,1,1,0,0,0,0,0,0,0,0,0,0]
          ,[0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0]
          ,[0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0]
          ,[0,0,0,0,0,0,0,0,-3,3,0,0,-2,-1,0,0]
          ,[0,0,0,0,0,0,0,0,2,-2,0,0,1,1,0,0]
          ,[-3,0,3,0,0,0,0,0,-2,0,-1,0,0,0,0,0]
          ,[0,0,0,0,-3,0,3,0,0,0,0,0,-2,0,-1,0]
          ,[9,-9,-9,9,6,3,-6,-3,6,-6,3,-3,4,2,2,1]
          ,[-6,6,6,-6,-3,-3,3,3,-4,4,-2,2,-2,-2,-1,-1]
          ,[2,0,-2,0,0,0,0,0,1,0,1,0,0,0,0,0]
          ,[0,0,0,0,2,0,-2,0,0,0,0,0,1,0,1,0]
          ,[-6,6,6,-6,-4,-2,4,2,-3,3,-3,3,-2,-1,-2,-1]
          ,[4,-4,-4,4,2,2,-2,-2,2,-2,2,-2,1,1,1,1]]

-- compute all rotations once
rotateImageS
  :: GrayImage -> V.Vector Double -> V.Vector GrayImage
rotateImageS img degs = rotatedImgs
  where !ds =
          computeDerivativeS . fromListUnboxed (Z :. ny :. nx) . pixelList $
          img
        (ny,nx) = dimensions img
        (centerX,centerY) = (fromIntegral nx / 2,fromIntegral ny / 2)
        !rotatedImgs =
          V.map (\rad ->
                   let mat =
                         VU.fromListN 4 $
                         P.map (\f -> f rad)
                               [cos,sin,\x -> -(sin x),cos]
                       newImg =
                         makeImage ny
                                   nx
                                   (\j i ->
                                      bicubicInterpolation ds matrixA $
                                      rotatePixel
                                        mat
                                        (centerY,centerX)
                                        (fromIntegral j,fromIntegral i)) :: GrayImage
                   in newImg)
                rads
        rads = V.map deg2Rad degs
        matrixA =
          V.fromListN 16 . P.map (VU.fromListN 16) $
          [[1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
          ,[0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0]
          ,[-3,3,0,0,-2,-1,0,0,0,0,0,0,0,0,0,0]
          ,[2,-2,0,0,1,1,0,0,0,0,0,0,0,0,0,0]
          ,[0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0]
          ,[0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0]
          ,[0,0,0,0,0,0,0,0,-3,3,0,0,-2,-1,0,0]
          ,[0,0,0,0,0,0,0,0,2,-2,0,0,1,1,0,0]
          ,[-3,0,3,0,0,0,0,0,-2,0,-1,0,0,0,0,0]
          ,[0,0,0,0,-3,0,3,0,0,0,0,0,-2,0,-1,0]
          ,[9,-9,-9,9,6,3,-6,-3,6,-6,3,-3,4,2,2,1]
          ,[-6,6,6,-6,-3,-3,3,3,-4,4,-2,2,-2,-2,-1,-1]
          ,[2,0,-2,0,0,0,0,0,1,0,1,0,0,0,0,0]
          ,[0,0,0,0,2,0,-2,0,0,0,0,0,1,0,1,0]
          ,[-6,6,6,-6,-4,-2,4,2,-3,3,-3,3,-2,-1,-2,-1]
          ,[4,-4,-4,4,2,2,-2,-2,2,-2,2,-2,1,1,1,1]]

rotateImageP :: ParallelParams
             -> GrayImage
             -> V.Vector Double
             -> IO (V.Vector GrayImage)
rotateImageP parallelParams img degs =
  do !ds <-
       computeDerivativeP . fromListUnboxed (Z :. ny :. nx) . pixelList $ img
     let (centerX,centerY) = (fromIntegral nx / 2,fromIntegral ny / 2)
         !rotatedImgs =
           parMapChunkVector
             parallelParams
             rseq
             (\rad ->
                let mat =
                      VU.fromListN 4 $
                      P.map (\f -> f rad)
                            [cos,sin,\x -> -(sin x),cos]
                    newImg =
                      makeImage ny
                                nx
                                (\j i ->
                                   bicubicInterpolation ds matrixA $
                                   rotatePixel mat
                                               (centerY,centerX)
                                               (fromIntegral j,fromIntegral i)) :: GrayImage
                in newImg)
             rads
     return rotatedImgs
  where (ny,nx) = dimensions img
        rads = V.map deg2Rad degs
        matrixA =
          V.fromListN 16 . P.map (VU.fromListN 16) $
          [[1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
          ,[0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0]
          ,[-3,3,0,0,-2,-1,0,0,0,0,0,0,0,0,0,0]
          ,[2,-2,0,0,1,1,0,0,0,0,0,0,0,0,0,0]
          ,[0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0]
          ,[0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0]
          ,[0,0,0,0,0,0,0,0,-3,3,0,0,-2,-1,0,0]
          ,[0,0,0,0,0,0,0,0,2,-2,0,0,1,1,0,0]
          ,[-3,0,3,0,0,0,0,0,-2,0,-1,0,0,0,0,0]
          ,[0,0,0,0,-3,0,3,0,0,0,0,0,-2,0,-1,0]
          ,[9,-9,-9,9,6,3,-6,-3,6,-6,3,-3,4,2,2,1]
          ,[-6,6,6,-6,-3,-3,3,3,-4,4,-2,2,-2,-2,-1,-1]
          ,[2,0,-2,0,0,0,0,0,1,0,1,0,0,0,0,0]
          ,[0,0,0,0,2,0,-2,0,0,0,0,0,1,0,1,0]
          ,[-6,6,6,-6,-4,-2,4,2,-3,3,-3,3,-2,-1,-2,-1]
          ,[4,-4,-4,4,2,2,-2,-2,2,-2,2,-2,1,1,1,1]]

rotatePixel :: VU.Vector Double
            -> (Double,Double)
            -> (Double,Double)
            -> (Double,Double)
rotatePixel mat (centerY,centerX) (y,x) = (y3,x3)
  where x1 = x - centerX
        y1 = y - centerY
        (y2,x2) =
          vecMatMult (y1,x1)
                     mat
        x3 = x2 + centerX
        y3 = y2 + centerY
