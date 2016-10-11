{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE TypeFamilies      #-}
module CV.Feature.PolarSeparable where

import           Control.DeepSeq
import           CV.CUDA.Context
import           CV.CUDA.DataType
import           CV.Feature
import           CV.Filter
import           CV.Filter.PolarSeparableFilter
import           CV.Utility.Parallel
import           Data.Array.Accelerate              as A
import           Data.Array.Accelerate.CUDA         as A
import           Data.Array.Accelerate.Data.Complex as A
import           Data.Array.Unboxed                 as AU
import           Data.Conduit
import           Data.Conduit.List                  as CL
import           Data.Vector.Unboxed                as VU
import           GHC.Float
import           Prelude                            as P

data PolarSeparableFeaturePoint = PolarSeparableFeaturePoint
  { x       :: Int
  , y       :: Int
  , feature :: VU.Vector Double
  } deriving (Show)

instance NFData PolarSeparableFeaturePoint where
  rnf (PolarSeparableFeaturePoint x y feature) = rnf x `seq` rnf y `seq` rnf feature

-- magnitudeConduit parallelParams CPU _filter =
--     error "CPU computing PolarSeparableFeaturePoint list is not supported."

magnitudeConduitFloat
  :: ParallelParams
  -> [Context]
  -> PolarSeparableFilter (Acc (A.Array DIM3 (A.Complex Float)))
  -> Conduit (AU.Array (Int, Int, Int) Float) IO [PolarSeparableFeaturePoint]
magnitudeConduitFloat parallelParams ctx filter = do
  xs <- CL.take (batchSize parallelParams)
  if P.length xs > 0
    then do
      let (_, (_, _, nfOld)) = bounds . P.head $ xs
          nfNew = getFilterNum . getParams $ filter
          radius = getRadius . getParams $ filter
          ys = P.map (\y -> P.map (slice2D y) [0 .. nfOld - 1]) xs
          zs =
            P.map
              (P.map toIArray .
               multiGPUStream ctx (applyFilter filter >-> A.map A.magnitude) .
               P.map fromIArray)
              ys :: [[AU.Array (Int, Int, Int) Float]]
          arrList =
            P.map
              (AU.array
                 ((0, 0, 0), (radius - 1, radius - 1, (nfOld * nfNew - 1))) .
               P.concat .
               P.zipWith
                 (\offset arr ->
                     P.map
                       (\((i, j, k), v) ->
                           ((i, j, k + offset * (radius ^ 2)), v)) .
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
                 (range ((0, 0), (radius - 1, radius - 1))) .
               slice1D)
              arrList
      sourceList result
      magnitudeConduitFloat parallelParams ctx filter
    else return ()


magnitudeConduitDouble
  :: ParallelParams
  -> [Context]
  -> PolarSeparableFilter (Acc (A.Array DIM3 (A.Complex Double)))
  -> Conduit (AU.Array (Int, Int, Int) Double) IO [PolarSeparableFeaturePoint]
magnitudeConduitDouble parallelParams ctx filter = do
  xs <- CL.take (batchSize parallelParams)
  if P.length xs > 0
    then do
      let (_, (_, _, nfOld)) = bounds . P.head $ xs
          nfNew = getFilterNum . getParams $ filter
          radius = getRadius . getParams $ filter
          ys = P.map (\y -> P.map (slice2D y) [0 .. nfOld - 1]) xs
          zs =
            P.map
              (P.map toIArray .
               multiGPUStream ctx (applyFilter filter >-> A.map A.magnitude) .
               P.map fromIArray)
              ys :: [[AU.Array (Int, Int, Int) Double]]
          arrList =
            P.map
              (AU.array
                 ((0, 0, 0), (radius - 1, radius - 1, (nfOld * nfNew - 1))) .
               P.concat .
               P.zipWith
                 (\offset arr ->
                     P.map
                       (\((i, j, k), v) ->
                           ((i, j, k + offset * (radius ^ 2)), v)) .
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
                 (range ((0, 0), (radius - 1, radius - 1))) .
               slice1D)
              arrList
      sourceList result
      magnitudeConduitDouble parallelParams ctx filter
    else return ()
