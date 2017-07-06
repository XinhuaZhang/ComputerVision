{-# LANGUAGE BangPatterns #-}
import           Control.Monad                          as M
import           CV.Statistics.PCA
import           CV.Utility.Parallel
import           Data.Array                             as Arr
import qualified Data.Image                             as IM
import           Data.List                              as L
import           Data.Vector                            as V
import           Data.Vector.Unboxed                    as VU
import           Graphics.Rendering.Chart.Backend.Cairo
import           Graphics.Rendering.Chart.Easy
import           Control.Monad.Parallel                 as MP
import           CV.Filter.PolarSeparableFilter
import           CV.Filter.CartesianGratingFilter

main = do
  let parallelParams =
        ParallelParams
        { numThread = 4
        , batchSize = 1
        }
      filterParams =
        PolarSeparableFilterParamsGrid
        { getPolarSeparableFilterGridRows = n
        , getPolarSeparableFilterGridCols = n
        , getPolarSeparableFilterGridScale = [0.2]
        , getPolarSeparableFilterGridRadialFreq = L.take 8 [0,1 ..]
        , getPolarSeparableFilterGridAngularFreq = L.take 8 [0,1 ..]
        }
      cFilterParams =
        CartesianGratingFilterParams
        { getCartesianGratingFilterRows = n
        , getCartesianGratingFilterCols = n
        , getCartesianGratingFilterScale = [pi]
        , getCartesianGratingFilterFreq = [0.1,0.2..0.8]
        , getCartesianGratingFilterAngle = [0, 30.. 180 - 30]
        }
      n = 128
      filterVecsList =
        getFilterExpansionList
          (makeFilterExpansion filterParams (div n 2) (div n 2) :: PolarSeparableFilterGridExpansion) -- L.++
        -- getFilterExpansionList
        --   (makeFilterExpansion cFilterParams (div n 2) (div n 2) :: CartesianGratingFilterExpansion)
      (!pcaMat, !eigenValueVector, _) =
        pcaSVD parallelParams (L.length filterVecsList) filterVecsList
  let eigenPairs = VU.toList . VU.imap (\i v -> (i, log v)) $ eigenValueVector
  print . L.length $ filterVecsList
  VU.imapM_ (\i v -> print (i, (log v))) $ eigenValueVector
  toFile def "EigenValue.png" $
    do layout_title .= "Eigenvalue"
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
