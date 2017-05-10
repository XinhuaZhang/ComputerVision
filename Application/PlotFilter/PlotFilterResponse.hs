import           Control.Arrow
import           Control.Monad                          as M
import           Control.Monad.Trans.Resource
import           CV.Array.Image
import           CV.Array.LabeledArray
import           CV.Feature.PolarSeparable
import           CV.Filter.GaussianFilter
import           CV.Filter.PolarSeparableFilter
import           CV.Utility.Parallel
import           CV.Utility.RepaArrayUtility            as RU
import           Data.Array                             as Arr
import           Data.Array.Repa                        as R
import           Data.Conduit
import           Data.Conduit.List                      as CL
import           Data.Image
import           Data.List                              as L
import           Data.Set                               as S
import           Graphics.Rendering.Chart.Backend.Cairo
import           Graphics.Rendering.Chart.Easy
import           System.Environment
import           Text.Printf
import Application.PinwheelPCANetMax.Pooling


main = do
  (inputPath:_) <- getArgs
  let (ny, nx) = (300, 300)
      parallelParams =
        ParallelParams
        { numThread = 4
        , batchSize = 4
        }
      filterParamsSet =
        PolarSeparableFilterParamsSet
        { getSizeSet = (ny, nx)
        , getDownsampleFactorSet = 1
        , getScaleSet = S.fromDistinctAscList [2]
        , getRadialFreqSet = S.fromDistinctAscList [0 .. (8 - 1)]
        , getAngularFreqSet = S.fromDistinctAscList [0 .. (8 - 1)]
        , getNameSet = Pinwheels
        }
      filterParamsList =
        L.concatMap generateMultilayerPSFParamsSet . L.tail . L.inits $
        [filterParamsSet]
  images <- readLabeledImageBinary inputPath 1
  filteredImg <-
    runResourceT
      (CL.sourceList images $$ CL.map (\(LabeledArray _ arr) -> arr) =$=
       singleLayerMagnitudeVariedSizedConduit parallelParams filterParamsSet =$=
       poolConduit parallelParams Max 3 =$= 
       CL.consume)
  let (Z :. nf :. ny :. nx) =
        extent . (\(LabeledArray _ arr) -> arr) . L.head $ images
      (Z :. nf' :. ny' :. nx') = extent . L.head $ filteredImg
      point = (150-2, 150-2)
      point1 = (100-2, 150-2) 
      -- point1 = (130-2, 210-2)
      width = 16
      startPoint =
        uncurry (\a b -> [a, b]) . join (***) (\x' -> x' - (div width 2)) $ point
      startPoint1 =
        uncurry (\a b -> [a, b]) . join (***) (\x' -> x' - (div width 2)) $ point1
      imagePatch =
        computeUnboxedS .
        RU.crop (startPoint L.++ [0]) [width, width, nf] .
        (\(LabeledArray _ arr) -> arr) . L.head $
        images
      xs =
        L.map
          (\k -> (\(j, i) -> (L.head filteredImg) R.! (Z :. k :. j :. i)) $ point) $
        [0 .. nf' - 1]
      xs1 =
        L.map
          (\k -> (\(j, i) -> (L.head filteredImg) R.! (Z :. k :. j :. i)) $ point1) $
        [0 .. nf' - 1]
      sortedPairs = L.reverse . L.sortOn snd . L.zip [(0 :: Int) ..] $ xs
      norm ys =
        let s = sqrt . L.sum . L.map (^ (2 :: Int)) $ ys
        in L.map (/ s) ys
  plotImage (show point L.++ "_Image.png") imagePatch
  toFile def (show point L.++ ".png") $
    do layout_title .= "Filter Response"
       M.mapM_
         (\(i, v) ->
             plot
               (line
                  (let as = S.toList . getAngularFreqSet $ filterParamsSet
                       rs = S.toList . getRadialFreqSet $ filterParamsSet
                       len = L.length as
                       j = mod i len
                       k = div i len
                   in printf "((%d,%d),%.3f)" (as L.!! j) (rs L.!! k) v :: String)
                  [L.zip [(0 :: Int) ..] $ L.replicate 100 v]))
         sortedPairs
  print . L.sum $ L.zipWith (*) (norm xs) (norm xs1)
  -- print . sqrt . L.sum . L.map (^2) $ xs
  -- print . sqrt . L.sum . L.map (^2) $ xs1
  -- print . sqrt . L.sum $
  --   L.zipWith (\a b -> (a - b) ^ (2 :: Int)) (norm xs) (norm xs1)
