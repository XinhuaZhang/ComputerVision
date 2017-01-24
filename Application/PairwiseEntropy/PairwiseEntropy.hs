{-# LANGUAGE FlexibleContexts #-}
module Application.PairwiseEntropy.PairwiseEntropy where

import           Application.PairwiseEntropy.Histogram
import           Control.Monad                         as M
import           Control.Monad.Trans.Resource
import           CV.Utility.Parallel
import           Data.Array.Repa                       as R
import           Data.Conduit
import           Data.Conduit.List                     as CL
import           Data.List                             as L
import           Data.Vector.Unboxed                   as VU

pairwiseEntropy
  :: (R.Source s Double)
  => Int -> Double -> R.Array s DIM3 Double -> [Double]
pairwiseEntropy nd bw arr =
  L.map
    (\(i, j) ->
        let s1 = R.slice arr (Z :. i :. All :. All)
            s2 = R.slice arr (Z :. j :. All :. All)
            pairList =
              L.map VU.fromList . L.transpose . L.map R.toList $
              [s1, s2]
            params = KdHistParams nd bw False 1
        in entropy $ build params pairList) $
  [ (i, j)
  | i <- [0 .. nf' - 1]
  , j <- [0 .. nf' - 1] ]
  where
    (Z :. nf' :. ny' :. nx') = extent arr


pairwiseEntropyConduit
  :: (R.Source s Double)
  => ParallelParams
  -> Int
  -> Double
  -> Conduit (Int, [R.Array s DIM3 Double]) (ResourceT IO) (Int, VU.Vector Double)
pairwiseEntropyConduit parallelParams nd bw = do
  xs <- CL.take (batchSize parallelParams)
  unless
    (L.null xs)
    (do let ys =
              parMapChunk
                parallelParams
                rdeepseq
                (\(label, zs) ->
                    ( label
                    , VU.concat $ L.concatMap (\z -> [VU.fromList $ pairwiseEntropy nd bw z]) zs))
                xs
        sourceList ys
        pairwiseEntropyConduit parallelParams nd bw)
