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
import Data.Vector.Unboxed as VU

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
      numTake = 8
      numDrop = 4
  images <- readLabeledImageBinary inputPath numTake
  let imgs = L.map (\(LabeledArray _ arr) -> arr) . L.drop numDrop $ images
      img1 = -- computeS . normalizeImage 255 . 
        computeS . extend (Z :. (1 :: Int) :. All :. All) . L.head $
        rotateSquare2DImageS
          [fromIntegral deg]
          (R.slice (L.head imgs) (Z :. (0 :: Int) :. All :. All))
      img2 = -- computeUnboxedS $ normalizeImage 255
              imgs L.!! 2
  M.zipWithM_ (\img i -> plotImage (show i L.++ ".png") img) imgs [1 ..]
  plotImage "2_1.png" img1
  print $ foldAllS max (fromIntegral (minBound :: Int)) img1
  print $ foldAllS min (fromIntegral (maxBound :: Word)) img1
  print $ foldAllS max (fromIntegral (minBound :: Int)) img2
  print $ foldAllS min (fromIntegral (maxBound :: Word)) img2
  print . R.sumAllS $ (img1 -^ img2)
  print . VU.take 10000 . toUnboxed $ (computeS $ img1 -^ img2)
