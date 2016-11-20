module Main where

import           CV.Filter                          as F
import           CV.Filter.PolarSeparableFilter
import           CV.Filter.PolarSeparableFilterAcc
import           CV.Filter.PolarSeparableFilterRepa
import           CV.Image as IM
import           Data.Array.Accelerate              as A
import           Data.Array.Accelerate.Data.Complex as A
import           Data.Array.Repa                    as R
import           Data.Complex                       as C
import           Data.Set                           as S
import           CV.CUDA.Context
import Prelude as P
import Data.List as L
import           Math.FFT
import           Data.Array.CArray              as CA

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
         filterEleList =
           [pixelList (IM.makeFilter 256
                                     256
                                     (getFilterFunc filterParams s rf af) :: ComplexImage)
           |rf <- [0..3]
           ,af <- [0..3]
           ,s <- [8]]
         nf = P.length filterEleList
         cArr1 =
           listArray ((0,0,0),(15,255,255)) . L.concat $
           filterEleList :: CArray (Int,Int,Int) (C.Complex Double)
         dftCArr1 = dftN [0,1] cArr1
     img <-
       readImage "/home/xinhua/Workspace/Polar-Separable-Filter/Dataset/Caltech101/Train/Original/Gray/0.pgm"
     ctx <- initializeGPUCtx (Option [0])
     let (ny,nx) = dimensions img
         imgAcc = A.fromList (A.Z A.:. ny A.:. nx) . pixelList $ img
         imgRepa =
           R.fromListUnboxed (R.Z R.:. 1 R.:. ny R.:. nx) . pixelList $ img :: R.Array U R.DIM3 Double
         filteredImgAcc = multiGPUStream ctx (applyFilter filterAcc >-> A.map A.magnitude) [imgAcc]
         filteredImgRepa = computeS . R.map C.magnitude . applyFilter filterRepa $ imgRepa :: R.Array U R.DIM3 Double
         cArr = threeDRArray2CArray imgRepa
         dftCArr = dftRCN [1,2] cArr
         rArr = threeDCArray2RArray dftCArr
         (R.Z R.:. nf R.:. ny' R.:. nx') = extent (getFilter filterRepa)  
         (R.Z R.:. n R.:. _ R.:. _) = extent rArr
         testArr = R.fromFunction
                       (R.Z R.:. (n * nf) R.:. ny' R.:. nx')
                       (\(R.Z R.:. k R.:. j R.:. i) ->
                          let k1 = div k nf
                              k2 = mod k nf
                              j' = if j <= (div ny' 2)
                                      then j
                                      else j - (div ny' 2) - 1
                          in (rArr R.! (R.Z R.:. k1 R.:. j' R.:. i)) *
                             ((getFilter filterRepa) R.! (R.Z R.:. k2 R.:. j R.:. i)))
     -- print . P.take 15 . A.toList . P.head $ filteredImgAcc
     print . (\x -> x P.!! 65536) . R.toList $ filteredImgRepa
     -- print . bounds $ cArr1
     -- print . bounds $ dftCArr1
     -- print . bounds $ cArr
     -- print . bounds $ dftCArr 
     -- -- print . extent $ rArr
     -- print (testArr R.! (R.Z R.:. 15 R.:. 255 R.:. 255))



