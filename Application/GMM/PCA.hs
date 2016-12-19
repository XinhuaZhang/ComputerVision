{-# LANGUAGE BangPatterns #-}

module Application.GMM.PCA where

import           Control.Arrow
import           Control.Monad
import           Control.Monad.Trans.Resource
import           CV.Utility.Parallel
import           Data.Array
import           Data.Binary
import           Data.ByteString.Lazy         as BL
import           Data.Conduit
import           Data.Conduit.List            as CL
import           Data.List                    as L
import           Data.Vector.Unboxed          as VU
import           Numeric.LinearAlgebra.Data   as LA
import           Numeric.Statistics.PCA
import           System.IO

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
  -> (Int, Int)
  -> Conduit [VU.Vector Double] (ResourceT IO) [VU.Vector Double]
pcaConduit parallelParams pcaMatrix (numDrop, numTake) = do
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
        sourceList .
          L.map
            (L.map (VU.fromList . LA.toList) .
             L.take numTake . L.drop numDrop . elems) $
          ys
        pcaConduit parallelParams pcaMatrix (numDrop, numTake))



pcaLabelConduit
  :: ParallelParams
  -> Matrix Double
  -> Conduit (Int, [VU.Vector Double]) (ResourceT IO) (Int, [VU.Vector Double])
pcaLabelConduit parallelParams pcaMatrix = do
  xs <- CL.take (batchSize parallelParams)
  unless
    (L.null xs)
    (do let !ys =
              parMapChunk
                parallelParams
                rdeepseq
                (second $
                 \x ->
                    pcaTransform
                      (listArray (1, L.length x) .
                       L.map (LA.fromList . VU.toList) $
                       x)
                      pcaMatrix)
                xs
        sourceList . L.map (second $ L.map (VU.fromList . LA.toList) . elems) $
          ys
        pcaLabelConduit parallelParams pcaMatrix)


writeMatrix :: FilePath -> Matrix Double -> IO ()
writeMatrix filePath mat =
  withBinaryFile filePath WriteMode $
  \h -> do
    let !bs = encode . LA.toLists $ mat
        !len = fromIntegral $ BL.length bs :: Word32
    BL.hPut h (encode len)
    BL.hPut h bs

readMatrix :: FilePath -> IO (Matrix Double)
readMatrix filePath =
  withBinaryFile filePath ReadMode $
  \h -> do
    lenbs <- BL.hGet h 4
    let !len = fromIntegral (decode lenbs :: Word32) :: Int
    bs <- BL.hGet h len
    return . LA.fromLists . decode $ bs
