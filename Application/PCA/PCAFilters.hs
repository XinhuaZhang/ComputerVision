{-# LANGUAGE BangPatterns #-}
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
import           Control.Monad.Parallel                 as MP

main = do
  let parallelParams = ParallelParams {numThread = 4, batchSize = 1}
      v4QuardTreeFilterParams =
        V4QuadTreeSeparableFilterParams
        { separableFilterQuadTreeLayer = 1
        , separableFilterRows = n
        , separableFilterCols = n
        , polarSeparableScale = [20]
        , polarSeparableRadialFreq = [12, 4, 8]
        , polarSeparableAngularFreq = [12, 4, 8]
        , polarSeparableName = Pinwheels
        , cartesianSeparableScale = [32]
        , cartesianSeparableXFreq = [0..11]
        , cartesianSeparableYFreq = [0..11]
        , hyperbolicSeparableScale = [32]
        , hyperbolicSeparableUFreq = [0..3]
        , hyperbolicSeparableVFreq = [0..9]
        , hyperbolicSeparableAngle = 15
        }
      n = 128
      filterVecsList =
        L.head . L.last $
        generateV4SeparableFilterQuadTreeFilter v4QuardTreeFilterParams
      (!pcaMat, !eigenValueVector, _) =
        pcaSVD parallelParams (L.length filterVecsList) filterVecsList
  let eigenPairs = VU.toList . VU.imap (\i v -> (i, log v)) $ eigenValueVector
  print . L.length $ filterVecsList
  VU.imapM_ (\i v -> print (i, (log v))) $ eigenValueVector
  toFile def "EigenValue.png" $ do
    layout_title .= "Eigenvalue"
    plot (line "" [eigenPairs])
  MP.sequence_ $
    L.zipWith
      (\i vec ->
         IM.writeImage ("Original_" L.++ show i L.++ ".ppm") .
         (\arr -> IM.arrayToImage arr :: IM.ComplexImage) .
         listArray ((0, 0), (n - 1, n - 1)) . VU.toList $
         vec)
      [0 ..]
      filterVecsList
  MP.sequence_ . V.toList $
    V.imap
      (\i vec ->
         IM.writeImage ("EigenVector_" L.++ show i L.++ ".ppm") .
         (\arr -> IM.arrayToImage arr :: IM.ComplexImage) .
         listArray ((0, 0), (n - 1, n - 1)) . VU.toList $
         vec) .
    pcaMatrix $
    pcaMat
