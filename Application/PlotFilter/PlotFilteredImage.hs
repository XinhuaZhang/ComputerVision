module Main where

import           Control.Monad                  as M
import           Control.Monad.Trans.Resource
import           CV.Array.Image
import           CV.Array.LabeledArray
import           CV.Feature.PolarSeparableRepa
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

main = do
  (inputPath:_) <- getArgs
  let (ny, nx) = (128, 128)
      parallelParams =
        ParallelParams
        { numThread = 4
        , batchSize = 4
        }
      filterParamsSet =
        PolarSeparableFilterParamsSet
        { getSizeSet = (ny, nx)
        , getDownsampleFactorSet = 1
        , getScaleSet = S.fromDistinctAscList [8]
        , getRadialFreqSet = S.fromDistinctAscList [0 .. (4 - 1)]
        , getAngularFreqSet = S.fromDistinctAscList [0 .. (4 - 1)]
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
       magnitudeVariedSizeConduit parallelParams filterParamsList 1 =$=
       CL.consume)
  let imgs = L.map (fromUnboxed (Z :. 1 :. ny :. nx)) . L.head $ filteredImg
  M.zipWithM
    (\img i -> plotImage (show i L.++ ".png") img)
    imgs
    [1 .. L.length imgs]
