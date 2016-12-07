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
  (withStrategy (parListChunk (div (P.length xs) nt) strat) . P.map f) xs
  
parZipWithChunk :: ParallelParams -> Strategy c -> (a -> b -> c) -> [a] -> [b] -> [c]
parZipWithChunk ParallelParams {numThread = nt} strat f xs =
  withStrategy (parListChunk (div (P.length xs) nt) strat) . P.zipWith f xs
  
parZipWith :: Strategy c -> (a -> b -> c) -> [a] -> [b] -> [c]
parZipWith strat f xs  = withStrategy (parList strat) . P.zipWith f xs

{- Boxed Vector -}
parMapVector
  :: Strategy b -> (a -> b) -> V.Vector a -> V.Vector b
parMapVector strat f = withStrategy (parTraversable strat) . V.map f

parMapChunkVector :: ParallelParams
                  -> Strategy b
                  -> (a -> b)
                  -> V.Vector a
                  -> V.Vector b
parMapChunkVector ParallelParams{numThread = nt} strat f xs =
  ((withStrategy
      (parVectorChunk (div (V.length xs) nt)
                      strat)) .
   (V.map f)) xs
  
parZipWithVector :: Strategy c
                 -> (a -> b -> c)
                 -> V.Vector a
                 -> V.Vector b
                 -> V.Vector c
parZipWithVector strat f xs =
  withStrategy (parTraversable strat) . V.zipWith f xs

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
  

parZipWith3Vector :: Strategy d
                  -> (a -> b -> c -> d)
                  -> V.Vector a
                  -> V.Vector b
                  -> V.Vector c
                  -> V.Vector d
parZipWith3Vector start f xs ys =
  withStrategy (parTraversable start) . V.zipWith3 f xs ys                       

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
  
parZipWith4ChunkVector :: ParallelParams
                       -> Strategy e
                       -> (a -> b -> c -> d -> e)
                       -> V.Vector a
                       -> V.Vector b
                       -> V.Vector c
                       -> V.Vector d -> V.Vector e
parZipWith4ChunkVector ParallelParams{numThread = nt} strat f xs ys zs as=
  (withStrategy
     (parVectorChunk (div (V.length xs) nt)
                     strat)) .
  (V.zipWith4 f xs ys zs) $
  as

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

