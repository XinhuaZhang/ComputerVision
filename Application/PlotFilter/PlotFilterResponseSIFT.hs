import           Application.GMM.PCA
import           Control.Arrow
import           Control.Monad                          as M
import           Control.Monad.Trans.Resource
import           CV.Array.Image
import           CV.Array.LabeledArray
import           CV.Feature.SIFT
import           CV.Filter.GaussianFilter
import           CV.Utility.Parallel
import           CV.Utility.RepaArrayUtility            as RU
import           Data.Array                             as Arr
import           Data.Array.Repa                        as R
import           Data.Conduit
import           Data.Conduit.List                      as CL
import           Data.Image
import           Data.List                              as L
import           Data.Set                               as S
import           Data.Vector.Unboxed                    as VU
import           Graphics.Rendering.Chart.Backend.Cairo
import           Graphics.Rendering.Chart.Easy
import           System.Environment
import           Text.Printf


main = do
  (inputPath:pcaFile:_) <- getArgs
  pcaMatrix <- readMatrix pcaFile
  let (ny, nx) = (300, 300)
      parallelParams =
        ParallelParams
        { numThread = 4
        , batchSize = 4
        }
      siftParams =
        SIFTParams
        { scaleSIFT = [2]
        , strideSIFT = 1
        }
      gaussianParams =
        L.map
          (\s ->
              GaussianFilterParams
              { getGaussianFilterSigma = s
              , getGaussianFilterSize = (0, 0)
              }) .
        scaleSIFT $
        siftParams
  images <- readLabeledImageBinary inputPath 1
  let (Z :. nf :. ny :. nx) =
        extent . (\(LabeledArray _ arr) -> arr) . L.head $ images
      imageArr = applyFilterVariedSize (L.head gaussianParams)  . (\(LabeledArray _ arr) -> arr) . L.head $ images
      point = (150, 150)
      point1 = (100, 150) -- (130, 210)
      width = 16
      xs =
        L.concatMap
          (\k ->
              VU.toList . getSIFTFeaturePointFromGradient point . getGradient $
              R.slice imageArr (Z :. k :. All :. All)) $
        [0 .. nf - 1]
      xs1 =
        L.concatMap
          (\k ->
              VU.toList . getSIFTFeaturePointFromGradient point1 . getGradient $
              R.slice imageArr (Z :. k :. All :. All)) $
        [0 .. nf - 1]
      sortedPairs = L.reverse . L.sortOn snd . L.zip [(0 :: Int) ..] $ xs
      norm ys =
        let s = sqrt . L.sum . L.map (^ (2 :: Int)) $ ys
        in L.map (/ s) ys
  ((zs:zs1:_):_) <-
    runResourceT $
    CL.sourceList [[VU.fromList xs, VU.fromList xs1]] $$
    pcaConduit parallelParams pcaMatrix =$=
    CL.consume
  toFile def (show point L.++ ".png") $
    do layout_title .= "Filter Response"
       M.mapM_
         (\(i, v) ->
             plot (line (show i) [L.zip [(0 :: Int) ..] $ L.replicate 100 v]))
         sortedPairs
  print . L.sum $ L.zipWith (*) xs xs1
  print . L.sum $ L.zipWith (*) (norm . VU.toList $ zs) (norm . VU.toList $ zs1)
