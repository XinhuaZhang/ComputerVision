import           Control.Monad          as M
import           CV.Filter.MorletWavelet
import           CV.Utility.Coordinates
import           Data.Array             as Arr
import qualified Data.Image             as IM
import           Data.List              as L
import           Data.Vector.Unboxed    as VU

main = do
  let n = 32
      m = 15
      filterParams =
        MorletWaveletParams
        { morletWaveletRows = n
        , morletWaveletCols = n
        , morletWaveletFreq = 3 * pi / 4
        , morletWaveletGaussianScale = pi
        , morletWaveletOrientation =
          [ 0 -- ,m .. 180 - m
          ]
        , morletWaveletScale = [1, 2, 3, 4]
        }
      filters =
        getFilterExpansionList
          (makeFilterExpansion filterParams (div n 2) (div n 2) :: MorletWaveletExpansion)
      imgList =
        L.map
          (IM.arrayToImage . listArray ((0, 0), (n - 1, n - 1)) . VU.toList)
          filters :: [IM.ComplexImage]
  M.zipWithM_
    (\i img -> IM.writeImage (show i L.++ ".pgm") $ IM.realPart img)
    [1 ..]
    imgList
