module Main where

import           Control.Monad       as M
import           CV.V4Filter         as V4
import           Data.Array          as Arr
import           Data.Image
import           Data.List           as L
import           Data.Set            as S
import           Data.Vector.Unboxed as VU


main = do
  let (ny, nx) = (128, 128)
      deg = 30
      filterParams =
        FourierMellinTransformParamsGrid
        { getFourierMellinTransformGridRows = ny
        , getFourierMellinTransformGridCols = nx
        , getFourierMellinTransformGridScale = [48]
        , getFourierMellinTransformGridRadialFreq = [2, 7, 12]
        , getFourierMellinTransformGridAngularFreq = [2, 7, 12]
        }
      filters =
        (\(FourierMellinTransform _ xs) -> L.concatMap L.concat xs) .
        getFilterVectors $
        (V4.makeFilter
           (PolarSeparableFilter filterParams Null)
           (div ny 2, div nx 2) :: FourierMellinTransformExpansionGrid)
      imgList =
        L.map
          (arrayToImage . listArray ((0, 0), (ny - 1, nx - 1)) . VU.toList)
          filters :: [ComplexImage]
  M.zipWithM_ (\i img -> writeImage (show i L.++ ".ppm") img) [1 ..] imgList
