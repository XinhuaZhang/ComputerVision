module Main where

import           Application.RotateDataset.RotationRepa
import           Control.Monad                          as M
import           Control.Monad.Trans.Resource
import           CV.Array.LabeledArray
import           CV.Feature.PolarSeparableRepa
import           CV.Filter.PolarSeparableFilter
import           CV.Utility.Parallel
import           Data.Array.Repa                        as R
import           Data.Conduit
import           Data.Conduit.List                      as CL
import           Data.List                              as L
import           Data.Set                               as S
import           Data.Vector.Unboxed                    as VU
import           Graphics.Rendering.Chart.Backend.Cairo
import           Graphics.Rendering.Chart.Easy
import           System.Environment


main = do
  (inputPath:_) <- getArgs
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
        , getRadialFreqSet = S.fromDistinctAscList [0 .. (8 - 1)]
        , getAngularFreqSet = S.fromDistinctAscList [0 .. (8 - 1)]
        , getNameSet = Pinwheels
        }
      deg = 36
  (img@(LabeledArray _ arr):_) <- readLabeledImageBinary inputPath 1
  rotatedLabeledImg <-
    CL.sourceList [img] $$
    rotateLabeledImageConduit parallelParams (fromIntegral deg) =$=
    CL.consume
  let (Z :. _ :. nRows :. numCols) = extent arr
      centerIdx = (div nRows 2) * numCols + (div numCols 2)
  mag <-
    runResourceT $
    (CL.sourceList rotatedLabeledImg $$
     labeledArrayMagnitudeSetVariedSizeConduit
       parallelParams
       [filterParamsSet1]
       1 =$=
     CL.consume)
  let centerMag =
        L.transpose $ L.map (\(_, vecs) -> L.map (VU.! centerIdx) vecs) mag
  toFile def "hehe.png" $
    do layout_title .= "Magnitude"
       plot
         (line
            ""
            (L.map (\xs -> L.zip [0 :: Int,deg .. (360 - deg)] xs) centerMag))
-- M.zipWithM_ (\xs i -> plot $ line (show i) [[(j, xs) | j <- [0,deg..(360 - deg)]]]) centerMag [1..]
