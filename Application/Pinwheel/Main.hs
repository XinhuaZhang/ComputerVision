import           Application.Pinwheel.Reconstruction
import           Control.Monad                       as M
import           Control.Monad.Trans.Resource
import           CV.Array.Image
import           CV.Array.LabeledArray
import           CV.Feature.PolarSeparable
import           CV.Filter.PolarSeparableFilter
import           CV.Utility.Parallel
import           CV.Utility.RepaArrayUtility         as RU
import           Data.Array                          as Arr
import           Data.Array.Repa                     as R
import           Data.Complex                        as C
import           Data.Conduit
import           Data.Conduit.List                   as CL
import           Data.Image
import           Data.List                           as L
import           Data.Set                            as S
import           System.Environment
import Application.RotateDataset.RotationRepa


main = do
  (inputPath:learningRate:threshold:count:_) <- getArgs
  let parallelParams =
        ParallelParams
        { numThread = 2
        , batchSize = 4
        }
      filterParamsSet =
        PolarSeparableFilterParamsSet
        { getSizeSet = (16, 16)
        , getDownsampleFactorSet = 1
        , getScaleSet = S.fromDistinctAscList [8]
        , getRadialFreqSet = S.fromDistinctAscList [0 .. (1 - 1)]
        , getAngularFreqSet = S.fromDistinctAscList [0 .. (8 - 1)]
        , getNameSet = Pinwheels
        }
      filters = (generateComplexFilters filterParamsSet)
  images <- readLabeledImageBinary inputPath 1
  let image = (\(LabeledArray _ arr) -> arr) . L.head $ images
      (Z :. imageNf :. _ :. _) = extent image
      point = (150, 150)
      imagePatch =
        RU.crop
          [fst point, snd point, 0]
          [ fst . getSizeSet $ filterParamsSet
          , snd . getSizeSet $ filterParamsSet
          , imageNf
          ]
          image
      rotatedImagePatches =
        rotate90Square2DImageS $
        R.slice imagePatch (Z :. (0 :: Int) :. All :. All)
      rotatedImagePatch = extend (Z :. (1 :: Int) :. All :. All) (rotatedImagePatches L.!! 1) 
      imgExtent = extent image
  act <-
    computeActivityComplex
      (read learningRate :: Double)
      (read threshold :: Double)
      (read count :: Int)
      filters
      imagePatch
  act1 <- computeActivityComplex
            (read learningRate :: Double)
            (read threshold :: Double)
            (read count :: Int)
            filters
            rotatedImagePatch
  plotImage "image.png" $ computeS imagePatch
  plotComplexImage ("Image_" L.++ show point) . R.map (:+ 0) $ imagePatch
  plotComplexImage ("Recon_" L.++ show point) . computeReconComplex filters $ act
  -- plotComplexImage ("RotatedImage_" L.++ show point) . R.map (:+ 0) $ rotatedImagePatch
  -- plotComplexImage ("RotatedRecon_" L.++ show point) . computeReconComplex filters $ act1
