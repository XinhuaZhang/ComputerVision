module Application.PlotFilter.Grid where

import           Codec.Picture
import           Data.Array.Repa as R
import           Data.List       as L
import           Data.Vector     as V

getGridImage :: Int -> Int -> [R.Array U DIM3 Double] -> DynamicImage
getGridImage cols boderLen xs =
  let maxValRGB =
        PixelRGB8 (maxBound :: Pixel8) (maxBound :: Pixel8) (maxBound :: Pixel8)
      (Z :. _ :. nyp' :. nxp') = extent . L.head $ xs
      numPixelBorderX = fromIntegral boderLen
      numPixelBorderY = fromIntegral boderLen
      nc = cols
      nr = div (L.length xs) cols
      gridImgHeight = nyp' + 2 * numPixelBorderY
      numPixelRows = nr * gridImgHeight
      gridImgWidth = nxp' + 2 * numPixelBorderX
      numPixelCols = nc * gridImgWidth
      ys = V.fromList xs
  in ImageRGB8 $
     generateImage
       (\i j ->
           let imgRowIdx = div j gridImgHeight
               imgColIdx = div i gridImgWidth
               pixelRowIdx = mod j gridImgHeight
               pixelColIdx = mod i gridImgWidth
               pixelRowImgIdx = pixelRowIdx - numPixelBorderY
               pixelColImgIdx = pixelColIdx - numPixelBorderX
           in if (pixelRowImgIdx < 0) ||
                 (pixelColImgIdx < 0) ||
                 (pixelRowImgIdx >= nyp') || (pixelColImgIdx >= nxp')
                then maxValRGB
                else let n = imgRowIdx * nc + imgColIdx
                         r =
                           fromIntegral
                             (round $
                              (ys V.! n) R.!
                              (Z :. 0 :. pixelRowImgIdx :. pixelColImgIdx) :: Int)
                         g =
                           fromIntegral
                             (round $
                              (ys V.! n) R.!
                              (Z :. 1 :. pixelRowImgIdx :. pixelColImgIdx) :: Int)
                         b =
                           fromIntegral
                             (round $
                              (ys V.! n) R.!
                              (Z :. 2 :. pixelRowImgIdx :. pixelColImgIdx) :: Int)
                     in if n > (L.length xs - 1)
                          then maxValRGB
                          else PixelRGB8 r g b)
       numPixelCols
       numPixelRows
