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
        , getFourierMellinTransformGridScale = [64]
        , getFourierMellinTransformGridRadialFreq = [0 .. 2]
        , getFourierMellinTransformGridAngularFreq = [0 .. 2]
        }
      filters =
        (\(FourierMellinTransform _ xs) -> L.concatMap L.concat xs) $
        getFilterVectors
          (V4.makeFilter
             (PolarSeparableFilter filterParams Null :: FourierMellinTransformExpansionGrid)
             (div nx 2, div ny 2))
      imgList =
        L.map
          (arrayToImage . listArray ((0, 0), (ny - 1, nx - 1)) . VU.toList)
          filters :: [ComplexImage]
  M.zipWithM_ (\i img -> writeImage (show i L.++ ".ppm") img) [1 ..] imgList
