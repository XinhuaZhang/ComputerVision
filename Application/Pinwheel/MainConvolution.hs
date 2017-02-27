{-# LANGUAGE TemplateHaskell #-}
import           Application.Pinwheel.Reconstruction
import           Control.Monad                       as M
import           Control.Monad.Trans.Resource
import           CV.Array.Image
import           CV.Array.LabeledArray
import           CV.Feature.PolarSeparable
import           CV.Feature.Coefficient as CO
import           CV.Filter.PolarSeparableFilter
import           CV.Utility.Parallel
import           CV.Utility.RepaArrayUtility         as RU
import           Data.Array                          as Arr
import           Data.Array.CArray                          as CA
import           Data.Array.Repa                     as R
import           Data.Complex                        as C
import           Data.Conduit
import           Data.Conduit.Binary                 as CB
import           Data.Conduit.List                   as CL
import           Data.Image
import           Data.List                           as L
import           Data.Set                            as S
import           System.Environment
import           Data.Array.CArray            as CA

main = do
  (inputPath:learningRate:threshold:count:_) <- getArgs
  let parallelParams =
        ParallelParams
        { numThread = 8
        , batchSize = 4
        }
      imageSize = (300, 225)
      filterParamsSet =
        PolarSeparableFilterParamsSet
        { getSizeSet = imageSize
        , getDownsampleFactorSet = 1
        , getScaleSet = S.fromDistinctAscList [1]
        , getRadialFreqSet = S.fromDistinctAscList [1 .. (4 - 0)]
        , getAngularFreqSet = S.fromDistinctAscList [0 .. (4 - 1)]
        , getNameSet = Pinwheels
        }
      (PolarSeparableFilter _ filters) = makeFilterSet filterParamsSet
      (PolarSeparableFilter _ filters180) = makeFlippedFilterSet filterParamsSet
  images <- readLabeledImageBinary inputPath 1
  let image = (\(LabeledArray _ arr) -> arr) . L.head $ images
      (Z :. imageNf :. _ :. _) = extent image
      imgExtent = extent image
      point = (0, 0)
      imagePatch =
        RU.crop
          [fst point, snd point, 0]
          [ snd . getSizeSet $ filterParamsSet
          , fst . getSizeSet $ filterParamsSet
          , imageNf
          ]
          image
  act <- computeCoefficientIO (read learningRate :: Double) filters filters180 imagePatch
  -- let (Z :. imgNf :. imgNy :. imgNx) = extent image
  --     (Z :. filterNf :. _ :. _) = extent filters
  -- initCoefficients <-
  --   M.replicateM imgNf .
  --   fmap (CA.listArray ((0, 0, 0), (filterNf - 1, imgNy - 1, imgNx - 1))) .
  --   M.replicateM (filterNf * imgNy * imgNx) . CO.generateComplexNumber $
  --   (-0.01, 0.01)
  let -- (e,act) = computeCoefficient (read learningRate :: Double) filters filters180 image initCoefficients
      act' = [threeDRArray2CArray act]
  -- print e
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
