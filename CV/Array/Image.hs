module CV.Array.Image where

import           Codec.Picture
import           Data.Array.Repa as R
import           Data.Image
import           Prelude         as P


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
  :: Double -> Array U DIM3 Double -> Array U DIM3 Double
normalizeImage upperBound img =
  computeS $ R.map (\x -> (x - minV) / (maxV - minV) * upperBound) img
  where maxV = foldAllS max 0 img
        minV = foldAllS min 10000 img

plotImage :: FilePath -> Array U DIM3 Double -> IO ()
plotImage filePath img =
  do let Z :. nfp' :. nyp' :. nxp' = extent img
         normalizedImg =
           normalizeImage (P.fromIntegral (maxBound :: Pixel8))
                          img
         w =
           case nfp' of
             1 ->
               ImageY8 $
               generateImage
                 (\i j ->
                    let v =
                          fromIntegral . round $
                          normalizedImg ! (Z :. 0 :. j :. i)
                    in v)
                 nxp'
                 nyp'
             3 ->
               ImageRGB8 $
               generateImage
                 (\i j ->
                    let r =
                          fromIntegral . round $
                          normalizedImg ! (Z :. 0 :. j :. i)
                        g =
                          fromIntegral . round $
                          normalizedImg ! (Z :. 1 :. j :. i)
                        b =
                          fromIntegral . round $
                          normalizedImg ! (Z :. 2 :. j :. i)
                    in PixelRGB8 r g b)
                 nxp'
                 nyp'
             _ ->
               error $
               "Image is neither a gray image nor a color image. There are " P.++
               show nfp' P.++
               " channels."
     savePngImage filePath w
