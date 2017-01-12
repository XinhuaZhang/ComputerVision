{-# LANGUAGE BangPatterns #-}
module Main where

import           Application.RotateDataset.RotationRepa
import           Control.Monad                          as M
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           CV.Array.LabeledArray
import           CV.Array.Image
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

readLabelFile :: FilePath -> IO [Int]
readLabelFile filePath =
  do bs <- readFile filePath
     return . L.map (\x -> read x :: Int) . lines $! bs

labelSource :: FilePath -> C.Source IO Int
labelSource filePath =
  do labels <- liftIO . readLabelFile $ filePath
     sourceList labels


main = do
  (inputPath:labelPath:degStr:_) <- getArgs
  let parallelParams =
        ParallelParams
        { numThread = 8
        , batchSize = 160
        }
      filterParamsSet1 =
        PolarSeparableFilterParamsSet
        { getSizeSet = (0, 0)
        , getDownsampleFactorSet = 1
        , getScaleSet = S.fromDistinctAscList [4]
        , getRadialFreqSet = S.fromDistinctAscList [0 .. (1 - 1)]
        , getAngularFreqSet = S.fromDistinctAscList [0 .. (8 - 1)]
        , getNameSet = Pinwheels
        }
      deg =
        if L.null degStr
          then 90
          else read degStr :: Int
  img <-
    imagePathSource inputPath $$ readImageConduit False =$=
    mergeSource (labelSource labelPath) =$=
    CL.map (\(label, img) -> LabeledArray label $ computeUnboxedS img) =$=
    CL.take 1
  rotatedLabeledImg45 <-
    CL.sourceList img $$ rescaleRotateLabeledImageConduit parallelParams 299 45 =$=
    CL.consume
  mag45 <-
    runResourceT $
    (CL.sourceList rotatedLabeledImg45 $$
     multiLayerMagnitudeVariedSizedConduit parallelParams [filterParamsSet1] 1 =$=
     CL.consume)
  rotatedLabeledImg90 <-
    CL.sourceList img $$ rescaleRotateLabeledImageConduit parallelParams 299 90 =$=
    CL.consume
  mag90 <-
    runResourceT $
    (CL.sourceList rotatedLabeledImg90 $$
     multiLayerMagnitudeVariedSizedConduit parallelParams [filterParamsSet1] 1 =$=
     CL.consume)
  let !ex@(Z :. _ :. nRows :. numCols) =
        extent $ (\(LabeledArray _ arr) -> arr) $ L.head $ rotatedLabeledImg45
      !centerIdx = (div nRows 2) * numCols + (div numCols 2)
      !centerMag45 =
        L.map (VU.toList . (flip L.genericIndex) centerIdx . L.head . snd) mag45
      !centerMag90 =
        L.map (VU.toList . (flip L.genericIndex) centerIdx . L.head . snd) mag90
  M.mapM_ print $ L.zip (centerMag45 !! 2) (centerMag90 !! 1)

