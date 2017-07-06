import           Control.Monad                  as M
import           CV.Filter.PolarSeparableFilter
import           CV.Utility.Coordinates
import           Data.Array                     as Arr
import qualified Data.Image                     as IM
import           Data.List                      as L
import           Data.Vector.Unboxed            as VU


main = do
  let n = 32
      m = 15
      filterParams =
        PolarSeparableFilterParamsGrid
        { getPolarSeparableFilterGridRows = n
        , getPolarSeparableFilterGridCols = n
        , getPolarSeparableFilterGridScale = [pi]
        , getPolarSeparableFilterGridRadialFreq = [0 .. 2]
        , getPolarSeparableFilterGridAngularFreq = [0 .. 2]
        }
      filters =
        getFilterExpansionList
          (makeFilterExpansion filterParams (div n 2) (div n 2) :: PolarSeparableFilterGridExpansion)
      imgList =
        L.map
          (IM.arrayToImage . listArray ((0, 0), (n - 1, n - 1)) . VU.toList)
          filters :: [IM.ComplexImage]
  M.zipWithM_
    (\i img -> IM.writeImage (show i L.++ ".pgm") img)
    [1 ..]
    imgList
