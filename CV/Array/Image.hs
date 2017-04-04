{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
module CV.Array.Image where

import           Codec.Picture
import           Control.Monad                as M
import           Control.Monad.Trans.Resource
import           CV.Utility.Parallel
import           CV.Utility.RepaArrayUtility  as RAU
import           Data.Array.Repa              as R
import           Data.Conduit
import           Data.Conduit.List            as CL
import           Data.Image
import           Data.List                    as L
import           Data.Vector.Unboxed          as VU
import           Data.Word
import           Prelude                      as P

grayImage2Array
  :: GrayImage -> Array U DIM3 Double
grayImage2Array img = fromListUnboxed (Z :. 1 :. ny :. nx) $ pixelList img
  where (ny,nx) = dimensions img

colorImage2Array
  :: ColorImage -> Array U DIM3 Double
colorImage2Array img =
  fromListUnboxed (Z :. 3 :. ny :. nx) $
  P.concatMap pixelList
              [r,g,b]
  where (ny,nx) = dimensions img
        (r,g,b) = colorImageToRGB img

normalizeImage
  :: Double -> Array U DIM3 Double -> Array D DIM3 Double
normalizeImage upperBound img =
  R.map (\x -> (x - minV) / (maxV - minV) * upperBound) img
  where
    maxV = foldAllS max (fromIntegral (minBound :: Int)) img
    minV = foldAllS min (fromIntegral (maxBound :: Word)) img

plotImage :: FilePath -> Array U DIM3 Double -> IO ()
plotImage filePath img = do
  let Z :. nfp' :. nyp' :. nxp' = extent img
      normalizedImg =
        computeUnboxedS $
        normalizeImage (P.fromIntegral (maxBound :: Pixel8)) img
      w =
        case nfp' of
          1 ->
            ImageY8 $
            generateImage
              (\i j ->
                 let v =
                       fromIntegral . round $ normalizedImg R.! (Z :. 0 :. j :. i)
                 in v)
              nxp'
              nyp'
          3 ->
            ImageRGB8 $
            generateImage
              (\i j ->
                 let r =
                       fromIntegral . round $ normalizedImg R.! (Z :. 0 :. j :. i)
                     g =
                       fromIntegral . round $ normalizedImg R.! (Z :. 1 :. j :. i)
                     b =
                       fromIntegral . round $ normalizedImg R.! (Z :. 2 :. j :. i)
                 in PixelRGB8 r g b)
              nxp'
              nyp'
          _ ->
            error $
            "Image is neither a gray image nor a color image. There are " P.++
            show nfp' P.++
            " channels."
  savePngImage filePath w

-- Set the maximum value of the maximum size, the ratio is intact.
{-# INLINE resize2DImageS #-}   

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
    !minVal = foldAllS min (fromIntegral (maxBound :: Word32)) arr
    !maxVal = 255 -- foldAllS max (fromIntegral (minBound :: Int)) arr
    !ds = computeDerivativeS . computeUnboxedS . delay $ arr

-- Set the maximum value of the maximum size, the ratio is intact.
resizeImageConduit
  :: (R.Source s Double)
  => ParallelParams
  -> Int
  -> Conduit (R.Array s DIM3 Double) (ResourceT IO) (R.Array U DIM3 Double)
resizeImageConduit parallelParams n = do
  xs <- CL.take (batchSize parallelParams)
  unless
    (L.null xs)
    (do let ys =
              parMapChunk
                parallelParams
                rseq
                (\x ->
                    let (Z :. nf' :. _ :. _) = extent x
                        (Z :. ny' :. nx') = extent . L.head $ zs
                        zs =
                          L.map
                            (\i ->
                                resize2DImageS n . R.slice x $
                                (Z :. i :. All :. All))
                            [0 .. nf' - 1]
                        arr = fromUnboxed (Z :. nf' :. ny' :. nx') .
                              VU.concat . L.map toUnboxed $
                              zs
                    in deepSeqArray arr arr)
                xs
        sourceList ys
        resizeImageConduit parallelParams n)


cropResizeImageConduit
  :: (R.Source s Double)
  => ParallelParams
  -> Int
  -> Conduit (R.Array s DIM3 Double) (ResourceT IO) (R.Array U DIM3 Double)
cropResizeImageConduit parallelParams n = do
  xs <- CL.take (batchSize parallelParams)
  unless
    (L.null xs)
    (do let ys =
              parMapChunk
                parallelParams
                rseq
                (\x ->
                   let (Z :. nf' :. ny' :. nx') = extent x
                       diff = div (abs $ ny' - nx') 2
                       y =
                         if ny' == nx'
                           then delay x
                           else if ny' > nx'
                                  then RAU.crop [0, diff, 0] [nx', nx', nf'] x
                                  else RAU.crop [diff, 0, 0] [ny', ny', nf'] x
                       zs =
                         L.map
                           (\i ->
                              resize2DImageS n . R.slice y $
                              (Z :. i :. All :. All))
                           [0 .. nf' - 1]
                       arr =
                         fromUnboxed (Z :. nf' :. n :. n) .
                         VU.concat . L.map toUnboxed $
                         zs
                   in deepSeqArray arr arr)
                xs
        sourceList ys
        cropResizeImageConduit parallelParams n)

padResizeImageConduit
  :: (R.Source s Double)
  => ParallelParams
  -> Int
  -> Conduit (R.Array s DIM3 Double) (ResourceT IO) (R.Array U DIM3 Double)
padResizeImageConduit parallelParams n = do
  xs <- CL.take (batchSize parallelParams)
  unless
    (L.null xs)
    (do let ys =
              parMapChunk
                parallelParams
                rseq
                (\x ->
                   let (Z :. nf' :. ny' :. nx') = extent x
                       maxSize = max ny' nx'
                       y = RAU.pad [maxSize, maxSize, nf'] x
                       zs =
                         L.map
                           (\i ->
                              resize2DImageS n . R.slice y $
                              (Z :. i :. All :. All))
                           [0 .. nf' - 1]
                       arr =
                         fromUnboxed (Z :. nf' :. n :. n) .
                         VU.concat . L.map toUnboxed $
                         zs
                   in deepSeqArray arr arr)
                xs
        sourceList ys
        padResizeImageConduit parallelParams n)
