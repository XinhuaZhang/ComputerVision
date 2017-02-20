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
import           Data.Conduit.Binary                 as CB
import           Data.Conduit.List                   as CL
import           Data.Image
import           Data.List                           as L
import           Data.Set                            as S
import           System.Environment

main = do
  (inputPath:learningRate:threshold:count:_) <- getArgs
  -- imageSize <-
  --   do xs <-
  --        runResourceT $
  --        sourceFile inputPath $$ readLabeledImagebinaryConduit =$=
  --        CL.take 1
  --      let (LabeledArray _ arr) = L.head xs
  --          (Z :. _ :. ny :. nx) = extent arr
  --      return (ny, nx)
  let parallelParams =
        ParallelParams
        { numThread = 8
        , batchSize = 4
        }
      imageSize = (200, 200)
      filterParamsSet =
        PolarSeparableFilterParamsSet
        { getSizeSet = imageSize
        , getDownsampleFactorSet = 1
        , getScaleSet = S.fromDistinctAscList [1,1.4,2]
        , getRadialFreqSet = S.fromDistinctAscList [1 .. (5 - 1)]
        , getAngularFreqSet = S.fromDistinctAscList [0 .. (4 - 1)]
        , getNameSet = Pinwheels
        }
      filterParamsSet180 =
        PolarSeparableFilterParamsSet
        { getSizeSet = imageSize
        , getDownsampleFactorSet = 1
        , getScaleSet = S.fromDistinctAscList [2]
        , getRadialFreqSet = S.fromDistinctAscList [0 .. (12 - 1)]
        , getAngularFreqSet = S.fromDistinctAscList [0 .. (12 - 1)]
        , getNameSet = Pinwheels180
        }
      filters = (generateComplexFilters filterParamsSet)
  -- filters180 = (generateComplexFilters filterParamsSet180)
  images <- readLabeledImageBinary inputPath 1
  -- filters <- generateRandomFilters imageSize 192
  let image = (\(LabeledArray _ arr) -> arr) . L.head $ images
      filters180 = L.map (computeS . flipArr) filters
      (Z :. imageNf :. _ :. _) = extent image
      imgExtent = extent image
      point = (0, 50)
      imagePatch =
        RU.crop
          [fst point, snd point, 0]
          [ fst . getSizeSet $ filterParamsSet
          , snd . getSizeSet $ filterParamsSet
          , imageNf
          ]
          image
  act <-
    computeActivityConvolution
      (read learningRate :: Double)
      (read threshold :: Double)
      (read count :: Int)
      filters
      filters180
      imagePatch
  -- plotImage "image.png" $ image
  -- plotComplexImage ("Image_") . R.map (:+ 0) $ image
  plotImage "image.png" $ computeS imagePatch
  plotComplexImage
    ("Image_" L.++ show (S.toList $ getScaleSet filterParamsSet) L.++ "_" L.++
     show (S.toList $ getAngularFreqSet filterParamsSet) L.++
     "x" L.++
     show (S.toList $ getRadialFreqSet filterParamsSet)) .
    R.map (:+ 0) $
    imagePatch
  plotComplexImages
    ("Recon_" L.++ show (S.toList $ getScaleSet filterParamsSet) L.++ "_" L.++
     show (S.toList $ getAngularFreqSet filterParamsSet) L.++
     "x" L.++
     show (S.toList $ getRadialFreqSet filterParamsSet)) .
    computeReconConvolution filters180 $
    act
