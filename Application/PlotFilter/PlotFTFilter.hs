import           Control.Monad       as M
import           CV.V4Filter         as V4
import           CV.V4FilterConvolution         
import           Data.Array          as Arr
import           Data.Image
import           Data.List           as L
import           Data.Vector.Unboxed as VU
import qualified Data.Complex as C

main = do
  let (ny, nx) = (128, 128)
      filterParams =
        FourierMellinTransformParamsGrid
        { getFourierMellinTransformGridRows = ny
        , getFourierMellinTransformGridCols = nx
        , getFourierMellinTransformGridScale = [2*pi]
        , getFourierMellinTransformGridRadialFreq = [8]
        , getFourierMellinTransformGridAngularFreq = [8]
        }
  ftFilters <-
    fourierTransformFilter (ny, nx) $
    getFilterVectors
      (V4.makeFilter
         (PolarSeparableFilter filterParams Null :: FourierMellinTransformExpansionGrid)
         (div nx 2, div ny 2))
  let filters =
        (\(FourierMellinTransformConvolution _ _ xs) -> L.concatMap L.concat xs)
          ftFilters
      imgList =
        L.map
          (arrayToImage . listArray ((0, 0), (ny - 1, nx - 1)) . VU.toList)
          filters :: [ComplexImage]
      imgList1 =
        L.map
          (arrayToImage . listArray ((0, 0), (ny - 1, nx - 1)) . VU.toList . VU.map C.magnitude)
          filters :: [GrayImage]
  M.zipWithM_ (\i img -> writeImage (show i L.++ ".ppm") img) [1 ..] imgList
  M.zipWithM_ (\i img -> writeImage (show i L.++ ".pgm") img) [1 ..] imgList1
