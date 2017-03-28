import           Control.Monad                          as M
import           CV.Statistics.PCA
import           CV.Utility.Parallel
import           CV.V4Filter
import           Data.Array                             as Arr
import qualified Data.Image                             as IM
import           Data.List                              as L
import           Data.Vector                            as V
import           Data.Vector.Unboxed                    as VU
import           Graphics.Rendering.Chart.Backend.Cairo
import           Graphics.Rendering.Chart.Easy

main = do
  let parallelParams =
        ParallelParams
        { numThread = 2
        , batchSize = 1
        }
      v4QuardTreeFilterParams =
        V4QuadTreeSeparableFilterParams
        { separableFilterQuadTreeLayer = 1
        , separableFilterRows = n
        , separableFilterCols = n
        , polarSeparableScale = [16]
        , polarSeparableRadialFreq = [16, 8, 4]
        , polarSeparableAngularFreq = [8, 8, 4]
        , polarSeparableName = Pinwheels
        , cartesianSeparableScale = [24]
        , cartesianSeparableXFreq = [0,0.05 .. 0.35]
        , cartesianSeparableYFreq = [0,0.05 .. 0.35]
        , hyperbolicSeparableScale = [24]
        , hyperbolicSeparableUFreq = [0,0.5 .. 3.5]
        , hyperbolicSeparableVFreq =
          0 : (L.map (\i -> 1 / (2 ^ i)) . L.reverse $ [0 .. 6])
        , hyperbolicSeparableAngle = 45
        }
      n = 128
      filterVecsList =
        L.head . L.head $ generateV4SeparableFilterQuadTreeFilter v4QuardTreeFilterParams
      (pcaMat, eigenValueVector, _) =
        pcaSVD parallelParams (L.length filterVecsList) filterVecsList
  let eigenPairs = VU.toList . VU.imap (\i v -> (i, v)) $ eigenValueVector
  print . L.length $ filterVecsList
  VU.imapM_ (\i v -> print (i, v)) $ eigenValueVector
  toFile def "EigenValue.png" $
    do layout_title .= "Eigenvalue"
       plot (line "" [eigenPairs])
  M.zipWithM_
    (\i vec ->
        IM.writeImage ("Original_" L.++ show i L.++ ".ppm") .
        (\arr -> IM.arrayToImage arr :: IM.ComplexImage) .
        listArray ((0, 0), (n - 1, n - 1)) . VU.toList $
        vec)
    [0 ..]
    filterVecsList
  V.imapM_
    (\i vec ->
        IM.writeImage ("EigenVector_" L.++ show i L.++ ".ppm") .
        (\arr -> IM.arrayToImage arr :: IM.ComplexImage) .
        listArray ((0, 0), (n - 1, n - 1)) . VU.toList $
        vec) .
    pcaMatrix $
    pcaMat
