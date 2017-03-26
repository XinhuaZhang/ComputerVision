import           Control.Monad       as M
import           CV.Statistics.PCA
import           CV.Utility.Parallel
import           CV.V4Filter
import           Data.Array          as Arr
import qualified Data.Image          as IM
import           Data.List           as L
import           Data.Vector         as V
import           Data.Vector.Unboxed as VU

main = do
  let parallelParams =
        ParallelParams
        { numThread = 2
        , batchSize = 1
        }
      v4QuardTreeFilterParams =
        V4FilterQuardTreeFilterParams
        { quardTreeLayer = 1
        , rows = n
        , cols = n
        , polarSeparableFilterScale = [16]
        , polarSeparableFilterRadialFreq = [16, 8, 4]
        , polarSeparableFilterAngularFreq = [8, 8, 4]
        , polarSeparableFilterName = Pinwheels
        , cartesianGratingFilterScale = [24]
        , cartesianGratingFilterFreq = [0.125, 0.25, 0.5, 1]
        , cartesianGratingFilterAngle = 10
        , hyperbolicFilterFilterScale = [24]
        , hyperbolicFilterFilterFreq = [0.125, 0.25, 0.5, 1]
        , hyperbolicFilterFilterAngle = 10
        }
      n = 128
      filterVecsList = L.head . L.head $ generateV4FilterQuardTreeFilter v4QuardTreeFilterParams
      (pcaMat, eigenValueVector, _) =
        pcaSVD parallelParams (L.length filterVecsList) filterVecsList
  V.imapM_
    (\i vec ->
        IM.writeImage (show i L.++ ".ppm") .
        (\arr -> IM.arrayToImage arr :: IM.ComplexImage) .
        listArray ((0, 0), (n - 1, n - 1)) . VU.toList $
        vec) .
    pcaMatrix $
    pcaMat
