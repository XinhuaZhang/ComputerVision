import           Application.PairwiseEntropy.Histogram  as H
import           Control.Monad                          as M
import           Control.Monad.Trans.Resource
import           CV.Array.Image
import           CV.Array.LabeledArray
import           CV.Feature.PolarSeparable
import           CV.Filter.PolarSeparableFilter
import           CV.IO.ImageIO
import           CV.Utility.Parallel
import           Data.Array.Repa                        as R
import           Data.Conduit                           as C
import           Data.Conduit.List                      as CL
import           Data.List                              as L
import           Data.Set                               as S
import           Data.Vector.Unboxed                    as VU
import           Graphics.Rendering.Chart.Backend.Cairo
import           Graphics.Rendering.Chart.Easy
import           System.Environment

main = do
  (inputPath:numImageStr:_) <- getArgs
  let numImage = read numImageStr :: Int
  images <- readLabeledImageBinary inputPath numImage
  let parallelParams =
        ParallelParams
        { numThread = 4
        , batchSize = 4
        }
      filterParamsSet1 =
        PolarSeparableFilterParamsSet
        { getSizeSet = (0, 0)
        , getDownsampleFactorSet = 1
        , getScaleSet = S.fromDistinctAscList [6]
        , getRadialFreqSet = S.fromDistinctAscList [0 .. (4 - 1)]
        , getAngularFreqSet = S.fromDistinctAscList [0 .. (4 - 1)]
        , getNameSet = Pinwheels
        }
  filteredImg <-
    runResourceT $
    CL.sourceList images $$ CL.map (\(LabeledArray _ arr) -> arr) =$=
    singleLayerMagnitudeVariedSizedConduit parallelParams filterParamsSet1 1 =$=
    CL.consume
  let (Z :. nf' :. _ :. _) = extent . L.head $ filteredImg
      histParams = KdHistParams 100 0.1 False 1
      deg = div 360 numImage
  M.mapM_
    (\i ->
        let hs =
              L.map
                (\arr ->
                    build histParams . L.map VU.fromList . L.transpose $
                    [R.toList . R.slice arr $ (Z :. i - 1 :. All :. All)])
                filteredImg
        in toFile def (show i L.++ ".png") $
           do layout_title .= "Histogram Intersection"
              plot
                (line
                   ""
                   [ L.zip (L.map (* (fromIntegral deg)) [0 .. numImage - 1]) $
                     L.map (H.intersection (L.head hs)) hs
                   ]))
    [1 .. nf']
