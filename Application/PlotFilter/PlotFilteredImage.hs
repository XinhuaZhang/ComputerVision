module Main where

import           Control.Monad                  as M
import           Control.Monad.Trans.Resource
import           CV.Array.Image
import           CV.Array.LabeledArray
import           CV.Feature.PolarSeparable
import           CV.Filter.PolarSeparableFilter
import           CV.Utility.Parallel
import           Data.Array                     as Arr
import           Data.Array.Repa                as R
import           Data.Conduit
import           Data.Conduit.List              as CL
import           Data.Image
import           Data.List                      as L
import           Data.Set                       as S
import           System.Environment
import CV.Filter.GaussianFilter

main = do
  (inputPath:_) <- getArgs
  let (ny, nx) = (300, 300)
      parallelParams =
        ParallelParams
        { numThread = 4
        , batchSize = 4
        }
      filterParamsSet =
        PolarSeparableFilterParamsSet
        { getSizeSet = (ny, nx)
        , getDownsampleFactorSet = 1
        , getScaleSet = S.fromDistinctAscList [2]
        , getRadialFreqSet = S.fromDistinctAscList [0 .. (8 - 1)]
        , getAngularFreqSet = S.fromDistinctAscList [1 .. (8 - 0)]
        , getNameSet = Pinwheels
        }
      filterParamsList =
        L.concatMap generateMultilayerPSFParamsSet . L.tail . L.inits $
        [filterParamsSet]
  images <- readLabeledImageBinary inputPath 1
  let (Z :. _ :. ny :. nx) =
        extent . (\(LabeledArray _ arr) -> arr) . L.head $ images
  filteredImg <-
    runResourceT
      (CL.sourceList images $$ CL.map (\(LabeledArray _ arr) -> arr) =$=
       singleLayerMagnitudeVariedSizedConduit parallelParams filterParamsSet =$=
       -- gaussianVariedSizeConduit parallelParams (GaussianFilterParams 2 undefined) =$=
       CL.consume)
  let (Z :. nf' :. ny' :. nx') = extent . L.head $ filteredImg
  plotImage "0.png" . (\(LabeledArray _ arr) -> arr) . L.head $ images
  M.mapM_
    (\i ->
        plotImage
          (show i L.++ ".png")
          (computeUnboxedS . extend (Z :. (1 :: Int) :. All :. All) $
           R.slice (L.head filteredImg) (Z :. (i - 1 :: Int) :. All :. All)))
    [1 .. nf']
