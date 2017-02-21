{-# LANGUAGE TemplateHaskell #-}
import           Application.Pinwheel.Reconstruction
import           Control.Monad                       as M
import           Control.Monad.Trans.Resource
import           CV.Array.Image
import           CV.Array.LabeledArray
import           CV.Feature.PolarSeparable
import           CV.Feature.Coefficient
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
        , getScaleSet = S.fromDistinctAscList [1]
        , getRadialFreqSet = S.fromDistinctAscList [1 .. (9 - 1)]
        , getAngularFreqSet = S.fromDistinctAscList [0 .. (8 - 1)]
        , getNameSet = Pinwheels
        }
      (PolarSeparableFilter _ filters) = makeFilterSet filterParamsSet
      (PolarSeparableFilter _ filters180) = makeFlippedFilterSet filterParamsSet
  images <- readLabeledImageBinary inputPath 1
  let image = (\(LabeledArray _ arr) -> arr) . L.head $ images
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
  act <- computeCoefficientIO (read learningRate :: Double) filters filters180 imagePatch
  let (Z :. actNf :. _ :. _) = extent act
      act' = [threeDRArray2CArray act]
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
    reconstruction filters180 $ act'
