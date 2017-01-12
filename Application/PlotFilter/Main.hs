module Main where

import           Control.Monad                  as M
import           CV.Filter.PolarSeparableFilter
import           Data.Array                     as Arr
import           Data.Image
import           Data.List                      as L
import           Data.Set                       as S


main = do
  let (ny, nx) = (299, 299)
      filterParamsSet =
        PolarSeparableFilterParamsSet
        { getSizeSet = (ny, nx)
        , getDownsampleFactorSet = 1
        , getScaleSet = S.fromDistinctAscList [4]
        , getRadialFreqSet = S.fromDistinctAscList [0 .. (8 - 1)]
        , getAngularFreqSet = S.fromDistinctAscList [0 .. (8 - 1)]
        , getNameSet = Pinwheels
        }
      filterParamsList = generatePSFParamsSet filterParamsSet
      centerY = div ny 2
      centerX = div nx 2
      filterEleList =
        L.map
          (\params@(PolarSeparableFilterParams _ downSampleFactor scale rf af _name) ->
              [ getFilterFunc params scale rf af (x - centerX) (y - centerY)
              | x <- [0 .. nx - 1]
              , y <- [0 .. ny - 1] ])
          filterParamsList
      imgList =
        L.map (arrayToImage . listArray ((0, 0), (ny - 1, nx - 1))) filterEleList :: [ComplexImage]
  zipWithM_ (\i img -> writeImage (show i L.++ ".ppm") img) [1 ..] imgList
