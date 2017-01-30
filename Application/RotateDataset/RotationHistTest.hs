import           Application.PairwiseEntropy.Histogram  as H
import           Application.RotateDataset.RotationRepa
import           Control.Monad                          as M
import           Control.Monad.IO.Class
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
  (inputPath:labelPath:degStr:_) <- getArgs
  images <-
    imagePathSource inputPath $$ readImageConduit False =$=
    mergeSource (labelSource labelPath) =$=
    CL.map (\(label, img) -> LabeledArray label $ computeUnboxedS img) =$=
    CL.take 1
  let parallelParams =
        ParallelParams
        { numThread = 4
        , batchSize = 4
        }
      filterParamsSet1 =
        PolarSeparableFilterParamsSet
        { getSizeSet = (0, 0)
        , getDownsampleFactorSet = 1
        , getScaleSet = S.fromDistinctAscList [2]
        , getRadialFreqSet = S.fromDistinctAscList [0 .. (4 - 1)]
        , getAngularFreqSet = S.fromDistinctAscList [0 .. (4 - 1)]
        , getNameSet = Pinwheels
        }
      deg = read degStr :: Int
      numImage = div 360 deg
  rotatedImg <-
    runResourceT $
    CL.sourceList images $$
    rescaleRotateLabeledImageConduit parallelParams 256 (fromIntegral deg) =$=
    CL.map (\(LabeledArray _ arr) -> arr) =$=
    CL.consume
  filteredImg <-
    runResourceT $
    CL.sourceList rotatedImg $$
    singleLayerMagnitudeVariedSizedConduit parallelParams filterParamsSet1 1 =$=
    CL.consume
  let (Z :. nf' :. _ :. _) = extent . L.head $ filteredImg
      histParamsRaw = KdHistParams 1000 0.001 False 1
      histParamsFiltered = KdHistParams 100 0.06 False 1
  -- M.zipWithM_
  --   (\i arr -> plotImage ("Img_" L.++ show i L.++ ".png") arr)
  --   [1 ..]
  --   rotatedImg
  -- print . VU.maximum . R.toUnboxed  . L.head $ filteredImg
  M.mapM_
    (\i ->
        let hs =
              L.map
                (\arr ->
                    build histParamsRaw . L.map VU.fromList . L.transpose $
                    [R.toList . R.slice arr $ (Z :. i - 1 :. All :. All)])
                rotatedImg
            intersectionVal = L.map (H.intersection (L.head hs)) hs
        in toFile def ("Raw_" L.++ show i L.++ ".png") $
           do layout_title .= "Histogram Intersection of Raw Pixel Values"
              plot
                (line
                   ""
                   [ L.zip
                       (L.map (* (fromIntegral deg)) [0 .. numImage - 1])
                       (L.map
                          (\x ->
                              (fromIntegral x :: Double) /
                              fromIntegral (L.head intersectionVal))
                          intersectionVal)
                   ]))
    [1 .. 1 :: Int]
  M.mapM_
    (\i ->
        let hs =
              L.map
                (\arr ->
                    build histParamsFiltered . L.map VU.fromList . L.transpose $
                    [R.toList . R.slice arr $ (Z :. i - 1 :. All :. All)])
                filteredImg
            intersectionVal = L.map (H.intersection (L.head hs)) hs
        in toFile def ("Filtered" L.++ show i L.++ ".png") $
           do layout_title .= "Histogram Intersection of Filtered Images"
              plot
                (line
                   ""
                   [ L.zip
                       (L.map (* (fromIntegral deg)) [0 .. numImage - 1])
                       (L.map
                          (\x ->
                              (fromIntegral x :: Double) /
                              fromIntegral (L.head intersectionVal))
                          intersectionVal)
                   ]))
    [1 .. nf']



readLabelFile :: FilePath -> IO [Int]
readLabelFile filePath =
  do bs <- readFile filePath
     return . L.map (\x -> read x :: Int) . lines $! bs

labelSource :: FilePath -> C.Source IO Int
labelSource filePath =
  do labels <- liftIO . readLabelFile $ filePath
     sourceList labels
