import           Control.Monad          as M
import           CV.Filter.Gabor
import           CV.Utility.Coordinates
import           Data.Array             as Arr
import qualified Data.Image             as IM
import           Data.List              as L
import           Data.Vector.Unboxed    as VU

main = do
  let n = 16
      m = 15
      filterParams =
        GaborFilterParams
        { gaborFilterRows = n
        , gaborFilterCols = n
        , gaborFilterFreq = [1 * pi / 64,3 * pi /4]
        , gaborFilterScale = [0.25 * pi, 0.5*pi]
        , gaborFilterOrientation = [0]
        }
      filters =
        getFilterExpansionList $
        (makeFilterExpansion filterParams (div n 2) (div n 2) :: GaborFilterExpansion)
      imgList =
        L.map
          (IM.arrayToImage . listArray ((0, 0), (n - 1, n - 1)) . VU.toList)
          filters :: [IM.ComplexImage]
  M.zipWithM_
    (\i img -> IM.writeImage (show i L.++ ".pgm") $ IM.realPart img)
    [1 ..]
    imgList
