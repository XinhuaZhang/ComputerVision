{-# LANGUAGE BangPatterns #-}

module Application.GMM.PCA where

import           Control.Monad
import           Control.Monad.Trans.Resource
import           CV.Utility.Parallel
import           Data.Array
import           Data.Conduit
import           Data.Conduit.List            as CL
import           Data.List                    as L
import           Data.Vector.Unboxed          as VU
import           Numeric.LinearAlgebra.Data   as LA
import           Numeric.Statistics.PCA

pcaSink :: Int -> Int -> Sink [VU.Vector Double] (ResourceT IO) (Matrix Double)
pcaSink numExample numPrincipal = do
  xs <- CL.take numExample
  if L.null xs
    then error "pcaSink: input data is empty."
    else do
      let !ys = L.map VU.concat . L.transpose $ xs
          !arr =
            listArray (1, L.length ys) . L.map (LA.fromList . VU.toList) $ ys
          !pcaMatrix = pcaN arr numPrincipal
      return pcaMatrix

pcaConduit
  :: ParallelParams
  -> Matrix Double
  -> Conduit [VU.Vector Double] (ResourceT IO) [VU.Vector Double]
pcaConduit parallelParams pcaMatrix = do
  xs <- CL.take (batchSize parallelParams)
  unless
    (L.null xs)
    (do let !ys =
              parMapChunk
                parallelParams
                rdeepseq
                (\x ->
                    pcaTransform
                      (listArray (1, L.length x) .
                       L.map (LA.fromList . VU.toList) $
                       x)
                      pcaMatrix)
                xs
        sourceList . L.map (L.map (VU.fromList . LA.toList) . elems) $ ys
        pcaConduit parallelParams pcaMatrix)
