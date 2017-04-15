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
        PolarSeparableFilterParamsGrid
        { getPolarSeparableFilterRows = ny
        , getPolarSeparableFilterCols = nx
        , getPolarSeparableFilterPolarFactor = 8
        , getPolarSeparableFilterScale = [16]
        , getPolarSeparableFilterFreq = [1 .. 8]
        , getPolarSeparableFilterAngle = [0,deg..90-deg]
        }
      filters =
        (\(V4PolarSeparableFilter _ xs) -> L.concat xs) . getFilterVectors $
        (V4.makeFilter
           (PolarSeparableFilter filterParams Null)
           (div ny 2, div nx 2) :: PolarSeparableFilterExpansion)
      imgList =
        L.map
          (arrayToImage . listArray ((0, 0), (ny - 1, nx - 1)) . VU.toList)
          filters :: [ComplexImage]
  M.zipWithM_ (\i img -> writeImage (show i L.++ ".ppm") img) [1 ..] imgList
