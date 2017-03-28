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
  let parallelParams = ParallelParams {numThread = 2, batchSize = 1}
      v4QuardTreeFilterParams =
        V4QuadTreeFilterParams
        { quadTreeLayer = 1
        , rows = n
        , cols = n
        , polarSeparableFilterScale = [16]
        , polarSeparableFilterRadialFreq = [16, 8, 4]
        , polarSeparableFilterAngularFreq = [8, 8, 4]
        , polarSeparableFilterName = Pinwheels
        , cartesianGratingFilterScale = [24]
        , cartesianGratingFilterFreq = L.map (\i -> 2 / ((sqrt 2) ^ i)) [0 .. 8]
        , cartesianGratingFilterAngle = 10
        , hyperbolicFilterFilterScale = [24]
        , hyperbolicFilterFilterFreq = L.map (\i -> 2 / ((sqrt 2) ^ i)) [0 .. 8]
        , hyperbolicFilterFilterAngle = 10
        }
      n = 128
      filterVecsList =
        L.head . L.head $
        generateV4FilterQuadTreeFilter v4QuardTreeFilterParams
      (pcaMat, eigenValueVector, _) =
        pcaSVD parallelParams (L.length filterVecsList) filterVecsList
  -- (pcaMat, eigenValueVector, _) <- pcaCovariance parallelParams (L.length filterVecsList) filterVecsList
  let eigenPairs = VU.toList . VU.imap (\i v -> (i, v)) $ eigenValueVector
  VU.imapM_ (\i v -> print (i, v)) $ eigenValueVector
  toFile def "EigenValue.png" $ do
    layout_title .= "Eigenvalue"
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
