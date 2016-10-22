module CV.Utility.Parallel
  ( module Control.Parallel.Strategies
  , module CV.Utility.Parallel
  ) where

import           Control.Parallel.Strategies
import           Data.Vector                 as V
import           Prelude                     as P

data ParallelParams = ParallelParams
  { numThread :: Int
  , batchSize :: Int
  } deriving (Show)

{-Parallel Functions-}
parMapChunk
  :: ParallelParams -> Strategy b -> (a -> b) -> [a] -> [b]
parMapChunk ParallelParams {numThread = nt} strat f xs =
  ((withStrategy (parListChunk (div (P.length xs) nt) strat)) . (P.map f)) xs

{- Boxed Vector -}
parMapChunkVector :: ParallelParams
                  -> Strategy b
                  -> (a -> b)
                  -> V.Vector a
                  -> V.Vector b
parMapChunkVector ParallelParams {numThread = nt} strat f xs =
  ((withStrategy (parVectorChunk (div (V.length xs) nt) strat)) . (V.map f)) xs
  
parZipWithChunkVector :: ParallelParams
                      -> Strategy c
                      -> (a -> b -> c)
                      -> V.Vector a
                      -> V.Vector b
                      -> V.Vector c
parZipWithChunkVector ParallelParams{numThread = nt} strat f xs ys =
  (withStrategy
     (parVectorChunk (div (V.length xs) nt)
                     strat)) .
  (V.zipWith f xs) $
  ys
  
parZipWith3ChunkVector :: ParallelParams
                       -> Strategy d
                       -> (a -> b -> c -> d)
                       -> V.Vector a
                       -> V.Vector b
                       -> V.Vector c
                       -> V.Vector d
parZipWith3ChunkVector ParallelParams{numThread = nt} strat f xs ys zs =
  (withStrategy
     (parVectorChunk (div (V.length xs) nt)
                     strat)) .
  (V.zipWith3 f xs ys) $
  zs

vectorChunk
  :: Int -> V.Vector a -> [V.Vector a]
vectorChunk n vec
  | V.null vec = []
  | otherwise = as : (vectorChunk n bs)
  where
    (as, bs) = V.splitAt n vec

parVectorChunk :: Int -> Strategy a -> Strategy (V.Vector a)
parVectorChunk n strat xs
  | n <= 1 = parTraversable strat xs
  | otherwise =
    fmap V.concat $ parTraversable (evalTraversable strat) (vectorChunk n xs)
