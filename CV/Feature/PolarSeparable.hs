{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module CV.Feature.PolarSeparable where

import           Control.DeepSeq                    as DS
import           Control.Monad.IO.Class
import           Control.Parallel
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
import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put
import           Data.Complex                       as C
import           Data.Conduit
import           Data.Conduit.List                  as CL
import           Data.Set                           as S
import           Data.Vector.Unboxed                as VU
import           GHC.Float
import           GHC.Generics
import           Prelude                            as P

data PolarSeparableFeaturePoint =
  PolarSeparableFeaturePoint {x       :: Int
                             ,y       :: Int
                             ,feature :: VU.Vector Double}
  deriving (Show,Read,Generic)

instance DS.NFData PolarSeparableFeaturePoint where
  rnf !_ = ()

instance Binary PolarSeparableFeaturePoint where
  put (PolarSeparableFeaturePoint x y feature) =
    do put x
       put y
       put $ VU.toList feature
  get =
    do x <- get :: Get Int
       y <- get :: Get Int
       feature <- get :: Get [Double]
       return (PolarSeparableFeaturePoint x
                                          y
                                          (VU.fromList feature))

normalizedMagnitudeConduitFloat
  :: ParallelParams
  -> [Context]
  -> PolarSeparableFilter (Acc (A.Array DIM3 (A.Complex Float)))
  -> Int
  -> Acc (A.Array DIM3 Float)
  -> Acc (A.Array DIM3 Float)
  -> Conduit (AU.Array (Int,Int,Int) Float) IO [PolarSeparableFeaturePoint]
normalizedMagnitudeConduitFloat parallelParams ctx filter factor meanArr varArr =
  do xs <- CL.take (batchSize parallelParams)
     if P.length xs > 0
        then do let (_,(nx',ny',nfOld)) = bounds . P.head $ xs
                    nx = nx' + 1
                    ny = ny' + 1
                    scale = getScale . getParams $ filter
                    nxNew = nx - (P.round $ (P.head $ S.toDescList scale) * 4)
                    nyNew = ny - (P.round $ (P.head $ S.toDescList scale) * 4)
                    ys =
                      P.map (\x ->
                               P.map (slice2D x)
                                     [0 .. nfOld])
                            xs
                    zs =
                      if factor == 1
                         then P.map (P.map toIArray .
                                     multiGPUStream
                                       ctx
                                       (applyFilter filter >->
                                        A.map A.magnitude >->
                                        A.zipWith3 (\m v x -> (x - m) / v)
                                                   meanArr
                                                   varArr >->
                                        crop25D (div (nx - nxNew) 2)
                                                (div (ny - nyNew) 2)
                                                nxNew
                                                nyNew
                                                nx
                                                ny) .
                                     P.map fromIArray)
                                    ys :: [[AU.Array (Int,Int,Int) Float]]
                         else P.map (P.map toIArray .
                                     multiGPUStream
                                       ctx
                                       (applyFilter filter >->
                                        A.map A.magnitude >->
                                        A.zipWith3 (\m v x -> (x - m) / v)
                                                   meanArr
                                                   varArr >->
                                        crop25D (div (nx - nxNew) 2)
                                                (div (ny - nyNew) 2)
                                                nxNew
                                                nyNew
                                                nx
                                                ny >->
                                        downsample25D factor) .
                                     P.map fromIArray)
                                    ys :: [[AU.Array (Int,Int,Int) Float]]
                    (lb,(sizeX,sizeY,nfNew)) = bounds . P.head . P.head $ zs
                    arrList =
                      parMapChunk
                        parallelParams
                        rseq
                        (AU.array (lb
                                  ,(sizeX,sizeY,((nfOld + 1) * (nfNew + 1) - 1))) .
                         P.concat .
                         P.zipWith (\offset arr ->
                                      P.map (\((i,j,k),v) ->
                                               ((i,j,k + offset * (nfNew + 1))
                                               ,v)) .
                                      AU.assocs $
                                      arr)
                                   [0,1 ..]) $!!
                      zs :: [AU.Array (Int,Int,Int) Float]
                    result =
                      parMapChunk
                        parallelParams
                        rdeepseq
                        (P.zipWith (\(x,y) z ->
                                      PolarSeparableFeaturePoint
                                        x
                                        y
                                        (VU.fromList . P.map float2Double $ z))
                                   (range ((0,0),(sizeX,sizeY))) .
                         slice1D)
                        arrList
                sourceList result
                liftIO $ performGCCtx ctx
                normalizedMagnitudeConduitFloat parallelParams ctx filter factor meanArr varArr
        else return ()

normalizedMagnitudeConduitDouble
  :: ParallelParams
  -> [Context]
  -> PolarSeparableFilter (Acc (A.Array DIM3 (A.Complex Double)))
  -> Int
  -> Acc (A.Array DIM3 Double)
  -> Acc (A.Array DIM3 Double)
  -> Conduit (AU.Array (Int,Int,Int) Double) IO [PolarSeparableFeaturePoint]
normalizedMagnitudeConduitDouble parallelParams ctx filter factor meanArr varArr =
  do xs <- CL.take (batchSize parallelParams)
     if P.length xs > 0
        then do let (_,(nx',ny',nfOld)) = bounds . P.head $ xs
                    nx = nx' + 1
                    ny = ny' + 1
                    scale = getScale . getParams $ filter
                    nxNew = nx - (P.round $ (P.head $ S.toDescList scale) * 4)
                    nyNew = ny - (P.round $ (P.head $ S.toDescList scale) * 4)
                    ys =
                      P.map (\x ->
                               P.map (slice2D x)
                                     [0 .. nfOld])
                            xs
                    zs =
                      if factor == 1
                         then P.map (P.map toIArray .
                                     multiGPUStream
                                       ctx
                                       (applyFilter filter >->
                                        A.map A.magnitude >->
                                        A.zipWith3 (\m v x -> (x - m) / v)
                                                   meanArr
                                                   varArr >->
                                        crop25D (div (nx - nxNew) 2)
                                                (div (ny - nyNew) 2)
                                                nxNew
                                                nyNew
                                                nx
                                                ny) .
                                     P.map fromIArray)
                                    ys :: [[AU.Array (Int,Int,Int) Double]]
                         else P.map (P.map toIArray .
                                     multiGPUStream
                                       ctx
                                       (applyFilter filter >->
                                        A.map A.magnitude >->
                                        A.zipWith3 (\m v x -> (x - m) / v)
                                                   meanArr
                                                   varArr >->
                                        crop25D (div (nx - nxNew) 2)
                                                (div (ny - nyNew) 2)
                                                nxNew
                                                nyNew
                                                nx
                                                ny >->
                                        downsample25D factor) .
                                     P.map fromIArray)
                                    ys :: [[AU.Array (Int,Int,Int) Double]]
                    (lb,(sizeX,sizeY,nfNew)) = bounds . P.head . P.head $ zs
                    arrList =
                      parMapChunk
                        parallelParams
                        rseq
                        (AU.array (lb
                                  ,(sizeX,sizeY,((nfOld + 1) * (nfNew + 1) - 1))) .
                         P.concat .
                         P.zipWith (\offset arr ->
                                      P.map (\((i,j,k),v) ->
                                               ((i,j,k + offset * (nfNew + 1))
                                               ,v)) .
                                      AU.assocs $
                                      arr)
                                   [0,1 ..]) $!!
                      zs :: [AU.Array (Int,Int,Int) Double]
                    result =
                      parMapChunk
                        parallelParams
                        rdeepseq
                        (P.zipWith (\(x,y) z ->
                                      PolarSeparableFeaturePoint x
                                                                 y
                                                                 (VU.fromList z))
                                   (range ((0,0),(sizeX,sizeY))) .
                         slice1D)
                        arrList
                sourceList result
                liftIO $ performGCCtx ctx
                normalizedMagnitudeConduitDouble parallelParams ctx filter factor meanArr varArr
        else return ()

magnitudeConduitFloat
  :: ParallelParams
  -> [Context]
  -> PolarSeparableFilter (Acc (A.Array DIM3 (A.Complex Float)))
  -> Int
  -> Conduit (AU.Array (Int,Int,Int) Float) IO [PolarSeparableFeaturePoint]
magnitudeConduitFloat parallelParams ctx filter factor =
  do xs <- CL.take (batchSize parallelParams)
     if P.length xs > 0
        then do let (_,(nx',ny',nfOld)) = bounds . P.head $ xs
                    nx = nx' + 1
                    ny = ny' + 1
                    scale = getScale . getParams $ filter
                    nxNew = nx - (P.round $ (P.head $ S.toDescList scale) * 4)
                    nyNew = ny - (P.round $ (P.head $ S.toDescList scale) * 4)
                    ys =
                      parMap rdeepseq
                             (\x ->
                                P.map (slice2D x)
                                      [0 .. nfOld])
                             xs
                    zs =
                      if factor == 1
                         then P.map (P.map toIArray .
                                     multiGPUStream
                                       ctx
                                       (applyFilter filter >->
                                        A.map A.magnitude >->
                                        crop25D (div (nx - nxNew) 2)
                                                (div (ny - nyNew) 2)
                                                nxNew
                                                nyNew
                                                nx
                                                ny) .
                                     P.map fromIArray)
                                    ys :: [[AU.Array (Int,Int,Int) Float]]
                         else P.map (P.map toIArray .
                                     multiGPUStream
                                       ctx
                                       (applyFilter filter >->
                                        A.map A.magnitude >->
                                        crop25D (div (nx - nxNew) 2)
                                                (div (ny - nyNew) 2)
                                                nxNew
                                                nyNew
                                                nx
                                                ny >->
                                        downsample25D factor) .
                                     P.map fromIArray)
                                    ys :: [[AU.Array (Int,Int,Int) Float]]
                    (lb,(sizeX,sizeY,nfNew)) = bounds . P.head . P.head $ zs
                    result =
                      parMapChunk
                        parallelParams
                        rdeepseq
                        (P.zipWith (\(x,y) z ->
                                      PolarSeparableFeaturePoint
                                        x
                                        y
                                        (VU.fromList . P.map float2Double $ z))
                                   (range ((0,0),(sizeX,sizeY))) .
                         slice1D .
                         AU.array (lb
                                  ,(sizeX,sizeY,((nfOld + 1) * (nfNew + 1) - 1))) .
                         P.concat .
                         P.zipWith (\offset arr ->
                                      P.map (\((i,j,k),v) ->
                                               ((i,j,k + offset * (nfNew + 1))
                                               ,v)) .
                                      AU.assocs $
                                      arr)
                                   [0,1 ..]) $!!
                      zs
                sourceList result
                magnitudeConduitFloat parallelParams ctx filter factor
        else return ()

magnitudeConduitDouble
  :: ParallelParams
  -> [Context]
  -> PolarSeparableFilter (Acc (A.Array DIM3 (A.Complex Double)))
  -> Int
  -> Conduit (AU.Array (Int,Int,Int) Double) IO [PolarSeparableFeaturePoint]
magnitudeConduitDouble parallelParams ctx filter factor =
  do xs <- CL.take (batchSize parallelParams)
     if P.length xs > 0
        then do let (_,(nx',ny',nfOld)) = bounds . P.head $ xs
                    nx = nx' + 1
                    ny = ny' + 1
                    scale = getScale . getParams $ filter
                    nxNew = nx - (P.round $ (P.head $ S.toDescList scale) * 4)
                    nyNew = ny - (P.round $ (P.head $ S.toDescList scale) * 4)
                    ys =
                      P.map (\x ->
                               P.map (slice2D x)
                                     [0 .. nfOld])
                            xs
                    zs =
                      if factor == 1
                         then P.map (P.map toIArray .
                                     multiGPUStream
                                       ctx
                                       (applyFilter filter >->
                                        A.map A.magnitude >->
                                        crop25D (div (nx - nxNew) 2)
                                                (div (ny - nyNew) 2)
                                                nxNew
                                                nyNew
                                                nx
                                                ny) .
                                     P.map fromIArray)
                                    ys :: [[AU.Array (Int,Int,Int) Double]]
                         else P.map (P.map toIArray .
                                     multiGPUStream
                                       ctx
                                       (applyFilter filter >->
                                        A.map A.magnitude >->
                                        crop25D (div (nx - nxNew) 2)
                                                (div (ny - nyNew) 2)
                                                nxNew
                                                nyNew
                                                nx
                                                ny >->
                                        downsample25D factor) .
                                     P.map fromIArray)
                                    ys :: [[AU.Array (Int,Int,Int) Double]]
                    (lb,(sizeX,sizeY,nfNew)) = bounds . P.head . P.head $ zs
                    arrList =
                      parMapChunk
                        parallelParams
                        rseq
                        (AU.array (lb
                                  ,(sizeX,sizeY,((nfOld + 1) * (nfNew + 1) - 1))) .
                         P.concat .
                         P.zipWith (\offset arr ->
                                      P.map (\((i,j,k),v) ->
                                               ((i,j,k + offset * (nfNew + 1))
                                               ,v)) .
                                      AU.assocs $
                                      arr)
                                   [0,1 ..]) $!!
                      zs :: [AU.Array (Int,Int,Int) Double]
                    result =
                      parMapChunk
                        parallelParams
                        rdeepseq
                        (P.zipWith (\(x,y) z ->
                                      PolarSeparableFeaturePoint x
                                                                 y
                                                                 (VU.fromList z))
                                   (range ((0,0),(sizeX,sizeY))) .
                         slice1D)
                        arrList
                sourceList result
                liftIO $ performGCCtx ctx
                magnitudeConduitDouble parallelParams ctx filter factor
        else return ()

complexConduitFloat
  :: ParallelParams
  -> [Context]
  -> PolarSeparableFilter (Acc (A.Array DIM3 (A.Complex Float)))
  -> Int
  -> Conduit (AU.Array (Int,Int,Int) Float) IO [PolarSeparableFeaturePoint]
complexConduitFloat parallelParams ctx filter factor =
  do xs <- CL.take (batchSize parallelParams)
     if P.length xs > 0
        then do let (_,(nx',ny',nfOld)) = bounds . P.head $ xs
                    nx = nx' + 1
                    ny = ny' + 1
                    scale = getScale . getParams $ filter
                    nxNew = nx - (P.round $ (P.head $ S.toDescList scale) * 4)
                    nyNew = ny - (P.round $ (P.head $ S.toDescList scale) * 4)
                    ys =
                      P.map (\x ->
                               P.map (slice2D x)
                                     [0 .. nfOld])
                            xs
                    zs =
                      if factor == 1
                         then P.map (P.map toIArray .
                                     multiGPUStream
                                       ctx
                                       (applyFilter filter >->
                                        crop25D (div (nx - nxNew) 2)
                                                (div (ny - nyNew) 2)
                                                nxNew
                                                nyNew
                                                nx
                                                ny) .
                                     P.map fromIArray)
                                    ys :: [[AU.Array (Int,Int,Int) (C.Complex Float)]]
                         else P.map (P.map toIArray .
                                     multiGPUStream
                                       ctx
                                       (applyFilter filter >->
                                        crop25D (div (nx - nxNew) 2)
                                                (div (ny - nyNew) 2)
                                                nxNew
                                                nyNew
                                                nx
                                                ny >->
                                        downsample25D factor) .
                                     P.map fromIArray)
                                    ys :: [[AU.Array (Int,Int,Int) (C.Complex Float)]]
                    (lb,(sizeX,sizeY,nfNew)) = bounds . P.head . P.head $ zs
                    arrList =
                      parMapChunk
                        parallelParams
                        rseq
                        (AU.array (lb
                                  ,(sizeX,sizeY,((nfOld + 1) * (nfNew + 1) - 1))) .
                         P.concat .
                         P.zipWith (\offset arr ->
                                      P.map (\((i,j,k),v) ->
                                               ((i,j,k + offset * (nfNew + 1))
                                               ,v)) .
                                      AU.assocs $
                                      arr)
                                   [0,1 ..]) $!!
                      zs :: [AU.Array (Int,Int,Int) (C.Complex Float)]
                    result =
                      parMapChunk
                        parallelParams
                        rdeepseq
                        (P.zipWith (\(x,y) z ->
                                      PolarSeparableFeaturePoint
                                        x
                                        y
                                        (VU.fromList .
                                         P.map float2Double .
                                         P.concatMap (\(a :+ b) -> [a,b]) $
                                         z))
                                   (range ((0,0),(sizeX,sizeY))) .
                         slice1D)
                        arrList
                sourceList result
                liftIO $ performGCCtx ctx
                complexConduitFloat parallelParams ctx filter factor
        else return ()

complexConduitDouble
  :: ParallelParams
  -> [Context]
  -> PolarSeparableFilter (Acc (A.Array DIM3 (A.Complex Double)))
  -> Int
  -> Conduit (AU.Array (Int,Int,Int) Double) IO [PolarSeparableFeaturePoint]
complexConduitDouble parallelParams ctx filter factor =
  do xs <- CL.take (batchSize parallelParams)
     if P.length xs > 0
        then do let (_,(nx',ny',nfOld)) = bounds . P.head $ xs
                    nx = nx' + 1
                    ny = ny' + 1
                    scale = getScale . getParams $ filter
                    nxNew = nx - (P.round $ (P.head $ S.toDescList scale) * 4)
                    nyNew = ny - (P.round $ (P.head $ S.toDescList scale) * 4)
                    ys =
                      P.map (\x ->
                               P.map (slice2D x)
                                     [0 .. nfOld])
                            xs
                    zs =
                      if factor == 1
                         then P.map (P.map toIArray .
                                     multiGPUStream
                                       ctx
                                       (applyFilter filter >->
                                        crop25D (div (nx - nxNew) 2)
                                                (div (ny - nyNew) 2)
                                                nxNew
                                                nyNew
                                                nx
                                                ny) .
                                     P.map fromIArray)
                                    ys :: [[AU.Array (Int,Int,Int) (C.Complex Double)]]
                         else P.map (P.map toIArray .
                                     multiGPUStream
                                       ctx
                                       (applyFilter filter >->
                                        crop25D (div (nx - nxNew) 2)
                                                (div (ny - nyNew) 2)
                                                nxNew
                                                nyNew
                                                nx
                                                ny >->
                                        downsample25D factor) .
                                     P.map fromIArray)
                                    ys :: [[AU.Array (Int,Int,Int) (C.Complex Double)]]
                    (lb,(sizeX,sizeY,nfNew)) = bounds . P.head . P.head $ zs
                    arrList =
                      parMapChunk
                        parallelParams
                        rseq
                        (AU.array (lb
                                  ,(sizeX,sizeY,((nfOld + 1) * (nfNew + 1) - 1))) .
                         P.concat .
                         P.zipWith (\offset arr ->
                                      P.map (\((i,j,k),v) ->
                                               ((i,j,k + offset * (nfNew + 1))
                                               ,v)) .
                                      AU.assocs $
                                      arr)
                                   [0,1 ..]) $!!
                      zs :: [AU.Array (Int,Int,Int) (C.Complex Double)]
                    result =
                      parMapChunk
                        parallelParams
                        rdeepseq
                        (P.zipWith (\(x,y) z ->
                                      PolarSeparableFeaturePoint
                                        x
                                        y
                                        (VU.fromList .
                                         P.concatMap (\(a :+ b) -> [a,b])
                                                     $z))
                                   (range ((0,0),(sizeX,sizeY))) .
                         slice1D)
                        arrList
                sourceList result
                liftIO $ performGCCtx ctx
                complexConduitDouble parallelParams ctx filter factor
        else return ()
