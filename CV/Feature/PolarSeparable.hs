{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
module CV.Feature.PolarSeparable where

import           Control.DeepSeq
import           Control.Monad.IO.Class
import           CV.CUDA.ArrayUtil
import           CV.CUDA.Context
import           CV.CUDA.DataType
import           CV.Feature
import           CV.Filter
import           CV.Filter.FilterStats
import           CV.Filter.PolarSeparableFilter
import           CV.Utility.Parallel
import           Data.Array.Accelerate              as A
import           Data.Array.Accelerate.CUDA         as A
import           Data.Array.Accelerate.Data.Complex as A
import           Data.Array.Unboxed                 as AU
import           Data.Conduit
import           Data.Conduit.List                  as CL
import           Data.Set                           as S
import           Data.Vector.Unboxed                as VU
import           GHC.Float
import           Prelude                            as P


data PolarSeparableFeaturePoint = PolarSeparableFeaturePoint
  { x       :: Int
  , y       :: Int
  , feature :: VU.Vector Double
  } deriving (Show,Read)

instance NFData PolarSeparableFeaturePoint where
  rnf (PolarSeparableFeaturePoint x y feature) = rnf x `seq` rnf y `seq` rnf feature

magnitudeConduitFloat
  :: ParallelParams
  -> [Context]
  -> PolarSeparableFilter (Acc (A.Array DIM3 (A.Complex Float)))
  -> Int -> Acc (A.Array DIM3 Float) -> Acc (A.Array DIM3 Float)
  -> Conduit (AU.Array (Int, Int, Int) Float) IO [PolarSeparableFeaturePoint]
magnitudeConduitFloat parallelParams ctx filter factor meanArr varArr = do
  xs <- CL.take (batchSize parallelParams)
  if P.length xs > 0
    then do
      let (_, (nx', ny', nfOld)) = bounds . P.head $ xs
          nx = nx' + 1
          ny = ny' + 1
          scale = getScale . getParams $ filter
          nxNew = nx - (P.round $ (P.head $ S.toDescList scale) * 4)
          nyNew = ny - (P.round $ (P.head $ S.toDescList scale) * 4)
          ys = P.map (\x -> P.map (slice2D x) [0 .. nfOld]) xs
          zs =
            if factor == 1
              then P.map
                     (P.map toIArray .
                      multiGPUStream
                        ctx
                        (applyFilter filter >-> A.map A.magnitude >->
                         A.zipWith3 (\m v x -> (x-m) / v) meanArr varArr >->
                         crop25D
                           (div (nx - nxNew) 2)
                           (div (ny - nyNew) 2)
                           nxNew
                           nyNew
                           nx
                           ny) .
                      P.map fromIArray)
                     ys :: [[AU.Array (Int, Int, Int) Float]]
              else P.map
                     (P.map toIArray .
                      multiGPUStream
                        ctx
                        (applyFilter filter >-> A.map A.magnitude >->
                         A.zipWith3 (\m v x -> (x - m ) / v) meanArr varArr >->
                         crop25D
                           (div (nx - nxNew) 2)
                           (div (ny - nyNew) 2)
                           nxNew
                           nyNew
                           nx
                           ny >->
                         downsample25D factor) .
                      P.map fromIArray)
                     ys :: [[AU.Array (Int, Int, Int) Float]]
          (lb, (sizeX, sizeY, nfNew)) = bounds . P.head . P.head $ zs
          arrList =
            P.map
              (AU.array (lb, (sizeX, sizeY, ((nfOld + 1) * (nfNew + 1) - 1))) .
               P.concat .
               P.zipWith
                 (\offset arr ->
                     P.map
                       (\((i, j, k), v) -> ((i, j, k + offset * (nfNew + 1)), v)) .
                     AU.assocs $
                     arr)
                 [0,1 ..])
              zs :: [AU.Array (Int, Int, Int) Float]
          result =
            parMapChunk
              parallelParams
              rdeepseq
              (P.zipWith
                 (\(x, y) z ->
                     PolarSeparableFeaturePoint
                       x
                       y
                       (VU.fromList . P.map float2Double $ z))
                 (range ((0, 0), (sizeX, sizeY))) .
               slice1D)
              arrList
      sourceList $!! result
      liftIO $ performGCCtx ctx
      magnitudeConduitFloat parallelParams ctx filter factor meanArr varArr
    else return ()


magnitudeConduitDouble
  :: ParallelParams
  -> [Context]
  -> PolarSeparableFilter (Acc (A.Array DIM3 (A.Complex Double)))
  -> Int
  -> Conduit (AU.Array (Int, Int, Int) Double) IO [PolarSeparableFeaturePoint]
magnitudeConduitDouble parallelParams ctx filter factor = do
  xs <- CL.take (batchSize parallelParams)
  if P.length xs > 0
    then do
      let (_, (nx', ny', nfOld)) = bounds . P.head $ xs
          nx = nx' + 1
          ny = ny' + 1
          scale = getScale . getParams $ filter
          nxNew = nx - (P.round $ (P.head $ S.toDescList scale) * 4)
          nyNew = ny - (P.round $ (P.head $ S.toDescList scale) * 4)
          ys = P.map (\x -> P.map (slice2D x) [0 .. nfOld]) xs
          zs =
            if factor == 1
              then P.map
                     (P.map toIArray .
                      multiGPUStream
                        ctx
                        (applyFilter filter >-> A.map A.magnitude >->
                         crop25D
                           (div (nx - nxNew) 2)
                           (div (ny - nyNew) 2)
                           nxNew
                           nyNew
                           nx
                           ny) .
                      P.map fromIArray)
                     ys :: [[AU.Array (Int, Int, Int) Double]]
              else P.map
                     (P.map toIArray .
                      multiGPUStream
                        ctx
                        (applyFilter filter >-> A.map A.magnitude >->
                         crop25D
                           (div (nx - nxNew) 2)
                           (div (ny - nyNew) 2)
                           nxNew
                           nyNew
                           nx
                           ny >->
                         downsample25D factor) .
                      P.map fromIArray)
                     ys :: [[AU.Array (Int, Int, Int) Double]]
          (lb, (sizeX, sizeY, nfNew)) = bounds . P.head . P.head $ zs
          arrList =
            P.map
              (AU.array (lb, (sizeX, sizeY, ((nfOld + 1) * (nfNew + 1) - 1))) .
               P.concat .
               P.zipWith
                 (\offset arr ->
                     P.map
                       (\((i, j, k), v) -> ((i, j, k + offset * (nfNew + 1)), v)) .
                     AU.assocs $
                     arr)
                 [0,1 ..])
              zs :: [AU.Array (Int, Int, Int) Double]
          result =
            parMapChunk
              parallelParams
              rdeepseq
              (P.zipWith
                 (\(x, y) z -> PolarSeparableFeaturePoint x y (VU.fromList z))
                 (range ((0, 0), (sizeX, sizeY))) .
               slice1D)
              arrList
      sourceList $!! result
      liftIO $ performGCCtx ctx
      magnitudeConduitDouble parallelParams ctx filter factor
    else return ()
