module Main where

import           Application.RotateDataset.RotationRepa
import           CV.Array.Image
import           CV.Utility.RepaArrayUtility            as RAU
import           Data.Array.Repa                        as R
import           Data.Image

main =
  do img <-
       readImage "/Users/xzhang/WorkSpace/ComputerVision/Application/RotateDataset/image_0001.pgm"
     let arr = grayImage2Array img
         (Z :. nf :. ny :. nx) = extent arr
         m = max ny nx
         paddImg = RAU.pad [m,m,nf] arr
         result =
           recaleAndRotate2DImageS 256
                                   36
                                   (slice arr (Z :. (0 :: Int) :. All :. All))
     -- plotImage "/Users/xzhang/WorkSpace/ComputerVision/Application/RotateDataset/test.png" (computeUnboxedS paddImg)
     plotImage "/Users/xzhang/WorkSpace/ComputerVision/Application/RotateDataset/test.png"
               (computeUnboxedS . extend (Z :. (1 :: Int) :. All :. All) $
                result) 
  
  
  
  
