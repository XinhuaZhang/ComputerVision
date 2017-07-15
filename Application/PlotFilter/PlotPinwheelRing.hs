import           Control.Monad          as M
import           CV.Filter.PinwheelRing
import           CV.Utility.Coordinates
import           Data.Array             as Arr
import qualified Data.Image             as IM
import           Data.List              as L
import           Data.Vector.Unboxed    as VU
import           Text.Printf

main = do
  let n = 128
      filterParams =
        PinwheelRingParams
        { pinwheelRingRows = n
        , pinwheelRingCols = n
        , pinwheelGaussianScale = 1
        , pinwheelRingScale = L.map (\x -> (sqrt 2) ** x) [0..0]
        , pinwheelRingRadialFreqs = 3/4*pi
        , pinwheelRingAngularFreqs = [5 .. 5]
        , pinwheelRingRadius = [1 .. 8]
        }
      filters =
        getFilterExpansionList
          (makeFilterExpansion filterParams (div n 2) (div n 2) :: PinwheelRingExpansion)
      imgList =
        L.map
          (IM.arrayToImage . listArray ((0, 0), (n - 1, n - 1)) . VU.toList)
          filters :: [IM.ComplexImage]
  M.zipWithM_
    (\i img -> IM.writeImage (printf "%03d.pgm" i)  img)
    [(1 :: Int) ..]
    imgList
