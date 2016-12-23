{-# LANGUAGE BangPatterns #-}
module Main where

import           Application.RotateDataset.RotationRepa
import           Control.Monad                          as M
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           CV.Array.LabeledArray
import           CV.Feature.PolarSeparableRepa
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

readLabelFile :: FilePath -> IO [Int]
readLabelFile filePath =
  do bs <- readFile filePath
     return . L.map (\x -> read x :: Int) . lines $! bs

labelSource :: FilePath -> C.Source IO Int
labelSource filePath =
  do labels <- liftIO . readLabelFile $ filePath
     sourceList labels


main = do
  (inputPath:labelPath:_) <- getArgs
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
      deg = 36 :: Int
  img <-
    imagePathSource inputPath $$ readImageConduit False =$=
    mergeSource (labelSource labelPath) =$=
    CL.map (\(label, img) -> LabeledArray label $ computeUnboxedS img) =$=
    CL.take 1
  rotatedLabeledImg <-
    CL.sourceList img $$
    rescaleRotateLabeledImageConduit parallelParams 255 (fromIntegral deg) =$=
    CL.consume
  let !ex@(Z :. _ :. nRows :. numCols) =
        extent $ (\(LabeledArray _ arr) -> arr) $ L.head $ rotatedLabeledImg
      !centerIdx = (div nRows 2) * numCols + (div numCols 2)
  print ex
  mag <-
    runResourceT $
    (CL.sourceList rotatedLabeledImg $$
     labeledArrayMagnitudeSetVariedSizeConduit
       parallelParams
       [filterParamsSet1]
       1 =$=
     CL.consume)
  let !centerMag =
        L.transpose $ L.map (\(_, vecs) -> L.map (VU.! centerIdx) vecs) mag
      !normlizedCenterMag = L.map (\x -> L.map (/ L.maximum x) x) centerMag
  toFile def "original.png" $
    do layout_title .= "Magnitude"
       M.zipWithM_
         (\xs i -> plot $ line (show i) [L.zip [0,deg .. (360 - deg)] xs])
         centerMag
         [1 ..]
  toFile def "normalized.png" $
    do layout_title .= "Magnitude"
       M.zipWithM_
         (\xs i -> plot $ line (show i) [L.zip [0,deg .. (360 - deg)] xs])
         normlizedCenterMag
         [1 ..]
