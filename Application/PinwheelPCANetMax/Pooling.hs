{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
module Application.PinwheelPCANetMax.Pooling
  ( PoolingType(..)
  , poolArr
  , poolConduit
  , poolGrid
  , poolGridList
  , gridNum
  ) where

import           Control.Monad                as M
import           Control.Monad.Trans.Resource
import           CV.Utility.Parallel
import           CV.Utility.RepaArrayUtility
import           Data.Array.Repa              as R
import           Data.Conduit
import           Data.Conduit.List            as CL
import           Data.List                    as L
import           Data.Vector.Unboxed          as VU
import           Prelude                      as P

data PoolingType
  = Max
  | Avg
  deriving (Show,Read)

{- CPU Pooling -}

{-# INLINE sumPoolList #-}

sumPoolList :: Int -> a -> (a -> a -> a) -> (a -> a -> a) -> [a] -> [a]
sumPoolList poolSize zero add sub xs =
  L.scanl' (\c (d, e) -> add (sub c d) e) (L.foldl' add zero as) (L.zip xs bs)
  where
    (as, bs) = L.splitAt poolSize xs

{-# INLINE maxPoolList #-}

maxPoolList
  :: (Ord a)
  => Int -> ([a] -> a) -> [a] -> [a]
maxPoolList poolSize maxOp ys
  | L.length as == poolSize = max' : maxPoolList poolSize maxOp (L.tail ys)
  | otherwise = [max']
  where
    (as, _bs) = L.splitAt poolSize ys
    !max' = maxOp as

{-# INLINE listOp #-}

listOp :: (a -> a -> a) -> [a] -> [a] -> [a]
listOp  = L.zipWith 

{-# INLINE avgPoolMatrix #-}
avgPoolMatrix
  :: (Fractional a)
  => Int -> [[a]] -> [[a]]
avgPoolMatrix poolSize =
  L.map
    (L.map (\y -> y / (fromIntegral poolSize ^ (2 :: Int))) .
     sumPoolList poolSize 0 (+) (-)) .
  sumPoolList poolSize (L.repeat 0) (listOp (+)) (listOp (-)) 
  
{-# INLINE maxPoolMatrix #-}

maxPoolMatrix :: (Ord a)
              => Int -> [[a]] -> [[a]]
maxPoolMatrix poolSize =
  L.map (maxPoolList poolSize L.maximum) .
  maxPoolList poolSize (L.foldl1' (L.zipWith max)) 

pool
  :: (Fractional a, Ord a)
  => PoolingType -> Int -> [[a]] -> [[a]]
pool Max = maxPoolMatrix
pool Avg = avgPoolMatrix


{-# INLINE poolArr #-}

poolArr
  :: (R.Source s e, Fractional e, Ord e, Unbox e)
  => PoolingType -> Int -> Array s DIM3 e -> Array U DIM3 e
poolArr poolingType poolingSize arr' = deepSeqArray y y
  where
    (Z :. nf' :. ny' :. nx') = extent arr'
    ss = L.map (\k -> R.toList $! R.slice arr' (Z :. k :. All :. All)) [0 .. nf']
    newNy = ny' - poolingSize + 1
    newNx = nx' - poolingSize + 1
    y =
      fromListUnboxed (Z :. nf' :. newNy :. newNx) .
      L.concat . pool poolingType poolingSize $
      ss

poolConduit
  :: (R.Source s e, Fractional e, Ord e, Unbox e)
  => ParallelParams
  -> PoolingType
  -> Int
  -> Conduit (Array s DIM3 e) (ResourceT IO) (Array U DIM3 e)
poolConduit parallelParams poolingType poolingSize = do
  xs <- CL.take (batchSize parallelParams)
  unless
    (L.null xs)
    (do let ys =
              parMapChunk
                parallelParams
                rseq
                (poolArr poolingType poolingSize)
                xs
        sourceList ys
        poolConduit parallelParams poolingType poolingSize)

{-# INLINE poolGrid #-}

poolGridList
  :: (R.Source s e, Unbox e)
  => Int
  -> Int
  -> (R.Array D DIM3 e -> VU.Vector e)
  -> R.Array s DIM3 e
  -> [VU.Vector e]
poolGridList poolSize stride f arr =
  [ f . cropUnsafe [0, i, j] [nf', poolSize, poolSize] $ arr
  | i <- startPointList nx'
  , j <- startPointList ny' ]
  where
    (Z :. ny' :. nx' :. nf') = extent arr
    startPointList len =
      L.filter (\i -> i + poolSize <= len) [0,stride .. len - 1]

poolGrid
  :: (R.Source s e, Unbox e)
  => Int
  -> Int
  -> (R.Array D DIM3 e -> VU.Vector e)
  -> R.Array s DIM3 e
  -> VU.Vector e
poolGrid poolSize stride f = VU.concat . poolGridList poolSize stride f 


gridNum :: Int -> Int -> Int -> Int -> Int
gridNum poolSize stride nx' ny' =
  L.length (startPointList nx') * L.length (startPointList ny')
  where
    startPointList len =
      L.filter (\i -> i + poolSize <= len) [0,stride .. len - 1]
