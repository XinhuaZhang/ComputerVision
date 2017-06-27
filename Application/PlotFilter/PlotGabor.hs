import           Control.Monad          as M
import           CV.Filter.Gabor
import           CV.Utility.Coordinates
import           Data.Array             as Arr
import qualified Data.Image             as IM
import           Data.List              as L
import           Data.Vector.Unboxed    as VU

main = do
  let n = 32
      m = 15
      filterParams =
        GaborFilterParams
        { gaborFilterRows = n
        , gaborFilterCols = n
        , gaborFilterFreq = [0.05,0.075,0.1,0.15,0.2,0.3,0.375]
        , gaborFilterScale = [2*pi]
        , gaborFilterOrientation = [0,m .. 180 - m]
        }
      filters =
        L.concatMap L.concat . (\(GaborFilterVectors x) -> x) . getFilterVectors $
        (makeFilter (Filter filterParams GaborFilterNull) (div n 2, div n 2) :: GaborFilter)
      imgList =
        L.map
          (IM.arrayToImage . listArray ((0, 0), (n - 1, n - 1)) . VU.toList)
          filters :: [IM.ComplexImage]
  M.zipWithM_ (\i img -> IM.writeImage (show i L.++ ".pgm") $ IM.realPart img) [1 ..] imgList
