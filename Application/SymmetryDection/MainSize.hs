import           Control.Monad               as M
import           CV.Filter.PinwheelWavelet
import           CV.IO.ImageIO
import           CV.Utility.Draw
import           CV.Utility.Parallel
import           CV.Utility.RepaArrayUtility
import           Data.Array.Repa             as R
import           Data.Complex
import           Data.List                   as L
import           Data.Vector.Storable        as VS
import           Data.Vector.Unboxed         as VU
import           System.Environment
import           Text.Printf

main = do
  let n = 128 -- 32 --128
      filterParams =
        PinwheelWaveletParams
        { pinwheelWaveletRows = n
        , pinwheelWaveletCols = n
        , pinwheelWaveletGaussianScale = 1
        , pinwheelWaveletScale = L.map (\x -> sqrt 2 ** x) [3 .. 3]
        , pinwheelWaveletRadialScale = L.map (\x -> (1 / sqrt 2) ** x) [0 .. 5]
        , pinwheelWaveletRadialFreqs = 3 / 4 * pi
        , pinwheelWaveletAngularFreqs = [-15 .. 15]
        , pinwheelWaveletRadius = [7 .. 7]
        }
  undefined
