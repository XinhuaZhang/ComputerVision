{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

module Application.RotateDataset.RotationRepa where

import           Control.DeepSeq
import           Control.Monad               as M
import           CV.Array.LabeledArray
import           CV.Utility.Coordinates
import           CV.Utility.Parallel
import           CV.Utility.RepaArrayUtility
import           Data.Array.Repa             as R
import           Data.Binary
import           Data.Conduit
import           Data.Conduit.List           as CL
import           Data.List                   as L
import           Data.Vector.Unboxed         as VU
import           Prelude                     as P

-- First pading image to be a square image then rotating it
recaleAndRotate2DImageS
  :: (R.Source s Double)
  => Int -> [Double] -> Array s DIM2 Double -> [Array U DIM2 Double]
recaleAndRotate2DImageS n degs arr =
  parMap
    rseq
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
              in if j' < 0 ||
                    j' > (fromIntegral n - 1) ||
                    i' < 0 || i' > (fromIntegral n - 1)
                   then 0
                   else bicubicInterpolation
                          ds
                          (minVal, maxVal)
                          (j' * ratio, i' * ratio)))
    degs
  where
    !minVal = foldAllS min (fromIntegral (maxBound :: Word64)) arr
    !maxVal = foldAllS max (fromIntegral (minBound :: Int)) arr
    !(Z :. ny :. nx) = extent arr
    !m =
      ceiling
        (sqrt . fromIntegral $ (nx ^ (2 :: Int) + ny ^ (2 :: Int)) :: Double)
    !paddedImg = pad [m, m] arr
    !ds = computeDerivativeS (computeUnboxedS paddedImg)
    !center = fromIntegral (n - 1) / 2
    !ratio = fromIntegral (m - 1) / fromIntegral (n - 1)

rotate2DImageS
  :: (R.Source s Double)
  => [Double] -> Array s DIM2 Double -> [Array U DIM2 Double]
rotate2DImageS degs arr =
  parMap
    rseq
    (\deg ->
        computeS $
        fromFunction
          (Z :. n :. n)
          (\(Z :. j :. i) ->
              bicubicInterpolation ds (minVal, maxVal) .
              rotatePixel
                (VU.fromListN 4 $
                 P.map (\f -> f (deg2Rad deg)) [cos, sin, \x -> -(sin x), cos])
                (center, center) $
              (fromIntegral j, fromIntegral i)))
    degs
  where
    !minVal = foldAllS min (fromIntegral (maxBound :: Word64)) arr
    !maxVal = foldAllS max (fromIntegral (minBound :: Int)) arr
    !(Z :. ny :. nx) = extent arr
    !n =
      ceiling
        (sqrt . fromIntegral $ (nx ^ (2 :: Int) + ny ^ (2 :: Int)) :: Double)
    !paddedImg = pad [n, n] arr
    !ds = computeDerivativeS (computeUnboxedS paddedImg)
    !center = fromIntegral (n - 1) / 2

-- Set the maximum value of the maximum size, the ratio is intact.
resize2DImageS
  :: (R.Source s Double)
  => Int -> Array s DIM2 Double -> Array U DIM2 Double
resize2DImageS n arr =
  computeS . fromFunction (Z :. newNy :. newNx) $
  \(Z :. j :. i) ->
     bicubicInterpolation
       ds
       (minVal, maxVal)
       (fromIntegral j * ratioY, fromIntegral i * ratioX)
  where
    !(Z :. ny :. nx) = extent arr
    !newNy =
      if ny >= nx
        then n
        else round
               (fromIntegral n * fromIntegral ny / fromIntegral nx :: Double)
    !newNx =
      if ny >= nx
        then round
               (fromIntegral n * fromIntegral nx / fromIntegral ny :: Double)
        else n
    !ratioX = fromIntegral (nx - 1) / fromIntegral (newNx - 1)
    !ratioY = fromIntegral (ny - 1) / fromIntegral (newNy - 1)
    !minVal = foldAllS min (fromIntegral (maxBound :: Word64)) arr
    !maxVal = foldAllS max (fromIntegral (minBound :: Int)) arr
    !ds = computeDerivativeS . computeUnboxedS . delay $ arr

{-# INLINE rotatePixel #-}

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

{-# INLINE vecMatMult #-}

vecMatMult :: (Double, Double) -> VU.Vector Double -> (Double, Double)
vecMatMult (x, y) vec = (a * x + c * y, b * x + d * y)
  where
    a = vec VU.! 0
    b = vec VU.! 1
    c = vec VU.! 2
    d = vec VU.! 3

rescaleRotateLabeledImageConduit
  :: ParallelParams
  -> Int
  -> Double
  -> Conduit (LabeledArray DIM3 Double) IO (LabeledArray DIM3 Double)
rescaleRotateLabeledImageConduit parallelParams n deg = do
  xs <- CL.take (batchSize parallelParams)
  unless
    (L.null xs)
    (do let ys =
              parMapChunk
                parallelParams
                rseq
                (\(LabeledArray label arr) ->
                    let (Z :. nf :. _ny :. _nx) = extent arr
                        !result =
                          if nf == 1
                            then L.map
                                   (LabeledArray label . computeUnboxedS .
                                    R.extend (Z :. (1 :: Int) :. All :. All)) .
                                 recaleAndRotate2DImageS n degs $
                                 R.slice arr (Z :. (0 :: Int) :. All :. All)
                            else L.map
                                   (\x ->
                                       LabeledArray label .
                                       fromUnboxed (Z :. nf :. n :. n) $!!
                                       VU.concat .
                                       L.map R.toUnboxed $
                                       x) .
                                 L.transpose .
                                 L.map
                                   (\i ->
                                       recaleAndRotate2DImageS n degs $
                                       R.slice arr (Z :. i :. All :. All)) $
                                 [0 .. nf - 1]
                    in result)
                xs
        sourceList . P.concat $ ys
        rescaleRotateLabeledImageConduit parallelParams n deg)
  where
    !len =
      if deg == 0
        then 1
        else round (360 / deg) :: Int
    !degs = L.map (* deg) [0 .. fromIntegral len - 1]

rotateLabeledImageConduit
  :: ParallelParams
  -> Double
  -> Conduit (LabeledArray DIM3 Double) IO (LabeledArray DIM3 Double)
rotateLabeledImageConduit parallelParams deg = do
  xs <- CL.take (batchSize parallelParams)
  unless
    (L.null xs)
    (do let ys =
              parMapChunk
                parallelParams
                rseq
                (\(LabeledArray label arr) ->
                    let (Z :. nf :. ny :. nx) = extent arr
                        !result =
                          if nf == 1
                            then L.map
                                   (LabeledArray label . computeUnboxedS .
                                    R.extend (Z :. (1 :: Int) :. All :. All)) .
                                 rotate2DImageS degs $
                                 R.slice arr (Z :. (0 :: Int) :. All :. All)
                            else L.map
                                   (\x ->
                                       LabeledArray label .
                                       fromUnboxed (Z :. nf :. ny :. nx) $!!
                                       VU.concat .
                                       L.map R.toUnboxed $
                                       x) .
                                 L.transpose .
                                 L.map
                                   (\i ->
                                       rotate2DImageS degs $
                                       R.slice arr (Z :. i :. All :. All)) $
                                 [0 .. nf - 1]
                    in result)
                xs
        sourceList . P.concat $ ys
        rotateLabeledImageConduit parallelParams deg)
  where
    !len =
      if deg == 0
        then 1
        else round (360 / deg) :: Int
    !degs = L.map (* deg) [0 .. fromIntegral len - 1]

resizeLabeledImageConduit
  :: ParallelParams
  -> Int
  -> Conduit (LabeledArray DIM3 Double) IO (LabeledArray DIM3 Double)
resizeLabeledImageConduit parallelParams n = do
  xs <- CL.take (batchSize parallelParams)
  unless
    (L.null xs)
    (do let ys =
              parMapChunk
                parallelParams
                rseq
                (\(LabeledArray label arr) ->
                    let !(Z :. nf :. ny :. nx) = extent arr
                        !result =
                          if nf == 1
                            then LabeledArray label . computeUnboxedS .
                                 R.extend (Z :. (1 :: Int) :. All :. All) .
                                 resize2DImageS n $
                                 R.slice arr (Z :. (0 :: Int) :. All :. All)
                            else LabeledArray label .
                                 fromUnboxed (Z :. nf :. ny :. nx) $!!
                                 VU.concat .
                                 L.map
                                   (\i ->
                                       R.toUnboxed . resize2DImageS n $
                                       R.slice arr (Z :. i :. All :. All)) $
                                 [0 .. nf - 1]
                    in result)
                xs
        sourceList ys
        resizeLabeledImageConduit parallelParams n)
