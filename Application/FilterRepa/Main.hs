module Main where

import           CV.Filter           as F
import           CV.Filter.PolarSeparableFilter
import           CV.Filter.PolarSeparableFilterRepa
import           CV.Image
import           Data.Array.Accelerate              as A
import           Data.Array.Accelerate.Data.Complex as A
import           Data.Array.Repa                    as R
import           Data.Complex                       as C
import           Data.Set                           as S

main =
  do let filterParams =
           PolarSeparableFilterParams {getRadius = 128
                                      ,getScale = S.fromDistinctAscList [8]
                                      ,getRadialFreq =
                                         S.fromDistinctAscList [0 .. (4 - 1)]
                                      ,getAngularFreq =
                                         S.fromDistinctAscList [0 .. (4 - 1)]
                                      ,getName = Pinwheels}
         filterAcc =
           F.makeFilter filterParams :: PolarSeparableFilter (Acc (A.Array A.DIM3 (A.Complex Double)))
         filterRepa =
           F.makeFilter filterParams :: PolarSeparableFilter (R.Array U R.DIM3 (C.Complex Double))
     img <- readImage "/home/xinhua/Dataset/101_ObjectCategories/watch/image_0021.pgm"
     let (ny,nx) = dimensions img
         imgAcc = A.use . A.fromList (A.Z A.:. ny A.:. nx) . pixelList $ img
         imgRepa = R.fromListUnboxed (R.Z R.:. 1 R.:. ny R.:. nx) . pixelList $ img :: R.Array U R.DIM3 Double
     undefined
