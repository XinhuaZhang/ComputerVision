import           Application.RotateDataset.RotationRepa
import           Control.Monad                          as M
import           CV.Array.Image
import           CV.Array.LabeledArray
import           CV.Filter.PolarSeparableFilter
import           CV.IO.ImageIO
import           CV.Utility.Parallel
import           Data.Array.Repa                        as R
import           Data.Conduit                           as C
import           Data.Conduit.List                      as CL
import           Data.List                              as L
import           Data.Set                               as S
import           System.Environment

main = do
  (inputPath:degStr:_) <- getArgs
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
      deg =
        if L.null degStr
          then 90
          else read degStr :: Int
  images <- readLabeledImageBinary inputPath 4
  let imgs = L.map (\(LabeledArray _ arr) -> arr) images
      img1 =
        L.head $
        rotate2DImageS
          [90]
          (slice (L.head imgs) (Z :. (0 :: Int) :. All :. All))
  M.zipWithM_ (\img i -> plotImage (show i L.++ ".png") img) imgs [1 ..]
  plotImage "2_1.png" (computeS $ extend (Z :. (1 :: Int) :. All :. All) img1)
