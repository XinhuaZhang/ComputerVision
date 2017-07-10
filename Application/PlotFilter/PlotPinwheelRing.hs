import           Control.Monad          as M
import           CV.Filter.PinwheelRing
import           CV.Utility.Coordinates
import           Data.Array             as Arr
import qualified Data.Image             as IM
import           Data.List              as L
import           Data.Vector.Unboxed    as VU

main = do
  let n = 128
      filterParams =
        PinwheelRingParams
        { pinwheelRingRows = n
        , pinwheelRingCols = n
        , pinwheelGaussianScale = 0.1
        , pinwheelRingScale = L.map (\x -> 2 ** x) [1 .. 1]
        , pinwheelRingRadialFreqs = 3 / 4 * pi
        , pinwheelRingAngularFreqs = [0..0]
        , pinwheelRingRadius = [1..63]
        }
      filters =
        getFilterExpansionList
          (makeFilterExpansion filterParams (div n 2) (div n 2) :: PinwheelRingExpansion)
      imgList =
        L.map
          (IM.arrayToImage . listArray ((0, 0), (n - 1, n - 1)) . VU.toList)
          filters :: [IM.ComplexImage]
  M.zipWithM_ (\i img -> IM.writeImage (show i L.++ ".pgm") img) [1 ..] imgList
