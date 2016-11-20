{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE TypeFamilies      #-}
module CV.Filter.PolarSeparableFilterStatisticsAcc where

import           Control.DeepSeq
import           Control.Monad.IO.Class         (liftIO)
import           Control.Parallel
import           CV.Filter
import           CV.Filter.FilterStats          as FS
import           CV.Filter.GaussianFilter
import           CV.Filter.PolarSeparableFilter
import           CV.Image                       as IM
import           CV.Utility.Coordinates
import           CV.Utility.Parallel
import           Data.Array.Unboxed             as AU
import           Data.Complex                   as C
import           Data.Conduit
import           Data.Conduit.List              as CL
import           Data.List                      as L
import           Data.Set                       as Set
import           GHC.Float
import           Prelude                        as P

instance CUDAStatistics (PolarSeparableFilter (Acc (A.Array DIM3 (A.Complex Float)))) where
  type GPUDataType (PolarSeparableFilter (Acc (A.Array DIM3 (A.Complex Float)))) = Float
  sink parallelParams ctx filePath filter =
    go 0 undefined (P.cycle [0]) (P.cycle [0])
    where
      go n size s1 s2 = do
        xs <- CL.take (batchSize parallelParams)
        if P.length xs > 0
          then let ctx1 =
                     if P.length ctx > 1
                       then P.init ctx
                       else ctx
                   ctx2 =
                     if P.length ctx > 1
                       then P.tail ctx
                       else ctx
                   (_, (ny', nx', nf)) = bounds . P.head $ xs
                   nx = nx' + 1
                   ny = ny' + 1
                   ys = P.map (\x -> P.map (slice2D x) [0 .. nf]) xs
                   zs1 =
                     P.map
                       (P.concatMap A.toList .
                        multiGPUStream
                          ctx1
                          (applyFilter filter >->
                           crop25D
                             (div ny 4)
                             (div nx 4)
                             (div ny 2)
                             (div nx 2)
                             ny
                             nx >->
                           FS.rotate3D >->
                           filterSum) .
                        P.map fromIArray)
                       ys
                   zs2 =
                     P.map
                       (P.concatMap A.toList .
                        multiGPUStream
                          ctx2
                          (applyFilter filter >->
                           crop25D
                             (div ny 4)
                             (div nx 4)
                             (div ny 2)
                             (div nx 2)
                             ny
                             nx >->
                           FS.rotate3D >->
                           filterSumSquare) .
                        P.map fromIArray)
                       ys
                   listSum = L.foldl' (L.zipWith (+))
                   ss1 = listSum s1 $!! zs1
                   ss2 = listSum s2 $!! zs2
               in par
                    ss1
                    (pseq ss2 (go (n + P.length xs) (div (nx * ny) 4) ss1 ss2))
          else let mean = P.map float2Double $ sampleMean s1 (n * size)
                   var =
                     (par
                        s1
                        (pseq
                           s2
                           (P.map float2Double $ sampleVar s1 s2 (n * size))))
               in do liftIO $ print . P.length $ mean
                     liftIO $
                       writeFilterStats
                         filePath
                         (par mean (pseq var (FilterStats mean var [])))



instance CUDAStatistics (PolarSeparableFilter (Acc (A.Array DIM3 (A.Complex Double)))) where
  type GPUDataType (PolarSeparableFilter (Acc (A.Array DIM3 (A.Complex Double)))) = Double
  sink parallelParams ctx filePath filter =
    go 0 undefined (P.cycle [0]) (P.cycle [0])
    where
      go n size s1 s2 = do
        xs <- CL.take (batchSize parallelParams)
        if P.length xs > 0
          then let ctx1 =
                     if P.length ctx > 1
                       then P.init ctx
                       else ctx
                   ctx2 =
                     if P.length ctx > 1
                       then P.tail ctx
                       else ctx
                   (_, (ny', nx', nf)) = bounds . P.head $ xs
                   nx = nx' + 1
                   ny = ny' + 1
                   ys = P.map (\x -> P.map (slice2D x) [0 .. nf]) xs
                   zs1 =
                     P.map
                       (P.concatMap A.toList .
                        multiGPUStream
                          ctx1
                          (applyFilter filter >->
                           crop25D
                             (div ny 4)
                             (div nx 4)
                             (div ny 2)
                             (div nx 2)
                             ny
                             nx >->
                           FS.rotate3D >->
                           filterSum) .
                        P.map fromIArray)
                       ys
                   zs2 =
                     P.map
                       (P.concatMap A.toList .
                        multiGPUStream
                          ctx2
                          (applyFilter filter >->
                           crop25D
                             (div ny 4)
                             (div nx 4)
                             (div ny 2)
                             (div nx 2)
                             ny
                             nx >->
                           FS.rotate3D >->
                           filterSumSquare) .
                        P.map fromIArray)
                       ys
                   listSum = L.foldl' (L.zipWith (+))
                   ss1 = listSum s1 $!! zs1
                   ss2 = listSum s2 $!! zs2
               in par
                    ss1
                    (pseq ss2 (go (n + P.length xs) (div (nx * ny) 4) ss1 ss2))
          else let mean = sampleMean s1 (n * size)
                   var = (par s1 (pseq s2 (sampleVar s1 s2 (n * size))))
               in liftIO $
                  writeFilterStats
                    filePath
                    (par mean (pseq var (FilterStats mean var [])))
