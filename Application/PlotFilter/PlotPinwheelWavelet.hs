import           Control.Monad          as M
import           CV.Filter.PinwheelWavelet
import           CV.Utility.Coordinates
import           Data.Array             as Arr
import qualified Data.Image             as IM
import           Data.List              as L
import           Data.Vector.Unboxed    as VU
import           Text.Printf

main = do
  let n = 32
      filterParams =
        PinwheelWaveletParams
        { pinwheelWaveletRows = n
        , pinwheelWaveletCols = n
        , pinwheelWaveletGaussianScale = 0.5 * pi 
        , pinwheelWaveletScale = L.map (\x -> 2 ** (x / 1)) [0..0]
        , pinwheelWaveletRadialScale = L.map (\x -> (1 / sqrt 2) ** x) [0..0]
        , pinwheelWaveletRadialFreqs = L.map (\x  -> x / 8 * pi) [5..5]
        , pinwheelWaveletAngularFreqs = [5 .. 5]
        , pinwheelWaveletRadius = [0..20]
        }
      filters =
        getFilterExpansionList
          (makeFilterExpansion filterParams (div n 2) (div n 2) :: PinwheelWaveletExpansion)
      imgList =
        L.map
          (IM.arrayToImage . listArray ((0, 0), (n - 1, n - 1)) . VU.toList)
          filters :: [IM.ComplexImage]
  M.zipWithM_
    (\i img -> IM.writeImage (printf "%03d.pgm" i)  img)
    [(1 :: Int) ..]
    imgList
