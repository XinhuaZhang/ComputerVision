{-# LANGUAGE BangPatterns #-}
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
import           Data.ByteString.Lazy        as BL
import           Data.Conduit
import           Data.Conduit.List           as CL
import qualified Data.Image                  as IM
import           Data.List                   as L
import           Data.Vector                 as V
import           Data.Vector.Unboxed         as VU
import           Prelude                     as P
import           System.IO

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
              bicubicInterpolation ds .
              rotatePixel
                (VU.fromListN 4 $
                 P.map (\f -> f (deg2Rad deg)) [cos, sin, \x -> -(sin x), cos])
                (center, center) $
              (fromIntegral j, fromIntegral i)))
    degs
  where
    (Z :. ny :. nx) = extent arr
    !n = round . sqrt . fromIntegral $ (nx ^ 2 + ny ^ 2)
    paddedImg = pad [n, n] arr
    ds = computeDerivativeS (computeUnboxedS paddedImg)
    !center = fromIntegral n / 2

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
                                   (LabeledArray label .
                                    computeUnboxedS .
                                    R.extend (Z :. (1 :: Int) :. All :. All)) .
                                 recaleAndRotate2DImageS n degs $
                                 R.slice arr (Z :. (0 :: Int) :. All :. All)
                            else L.map
                                   (LabeledArray label .
                                    fromUnboxed (Z :. nf :. n :. n) .
                                    VU.concat . L.map R.toUnboxed) .
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
        else round (360 / deg)
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
                                   (LabeledArray label .
                                    computeUnboxedS .
                                    R.extend (Z :. (1 :: Int) :. All :. All)) .
                                 rotate2DImageS degs $
                                 R.slice arr (Z :. (0 :: Int) :. All :. All)
                            else L.map
                                   (LabeledArray label .
                                    fromUnboxed (Z :. nf :. ny :. nx) .
                                    VU.concat . L.map R.toUnboxed) .
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
        else round (360 / deg)
    !degs = L.map (* deg) [0 .. fromIntegral len - 1]
