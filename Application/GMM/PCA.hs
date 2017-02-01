{-# LANGUAGE BangPatterns #-}

module Application.GMM.PCA where

import           Control.Arrow
import           Control.Monad                as M
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           CV.Feature.PolarSeparable
import           CV.Utility.Parallel
import           CV.Utility.RepaArrayUtility
import           Data.Array
import           Data.Array.Repa              as R
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
      let !ys = L.concat xs
          !arr' =
            listArray (1, VU.length $ L.head ys) .
            L.map LA.fromList . L.transpose . L.map VU.toList $
            ys
          !pcaMatrix = pcaN arr' numPrincipal
      return pcaMatrix

hPCASink
  :: Handle
  -> Int
  -> Int -> Int
  -> Sink (R.Array U DIM3 Double) (ResourceT IO) [R.Array U DIM3 Double]
hPCASink h numExample numPrincipal downsampleFactor' = do
  arrs <- CL.take numExample
  if L.null arrs
    then error "pcaSink: input data is empty."
    else do
      let !xs =
            L.map
              (extractPointwiseFeature .
               downsample [downsampleFactor', downsampleFactor', 1])
              arrs
          !ys = L.concat xs
          !arr' =
            listArray (1, VU.length $ L.head ys) .
            L.map LA.fromList . L.transpose . L.map VU.toList $
            ys
          !pcaMatrix = pcaN arr' numPrincipal
      liftIO $ hPutMatrix h pcaMatrix
      liftIO . System.IO.putStrLn $ "One layer is finished."
      return arrs

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
                (\x' ->
                    pcaTransform
                      (listArray (1, VU.length . L.head $ x') .
                       L.map LA.fromList . L.transpose . L.map VU.toList $
                       x')
                      pcaMatrix)
                xs
        sourceList .
          L.map (L.map VU.fromList . L.transpose . L.map LA.toList . elems) $
          ys
        pcaConduit parallelParams pcaMatrix)



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
                 \x' ->
                    pcaTransform
                      (listArray (1, VU.length . L.head $ x') .
                       L.map LA.fromList . L.transpose . L.map VU.toList $
                       x')
                      pcaMatrix)
                xs
        sourceList .
          L.map
            (second $ L.map VU.fromList . L.transpose . L.map LA.toList . elems) $
          ys
        pcaLabelConduit parallelParams pcaMatrix)
        

pcaLabelMultiLayerConduit
  :: ParallelParams
  -> [Matrix Double]
  -> Conduit (Int, [[VU.Vector Double]]) (ResourceT IO) (Int, [[VU.Vector Double]])
pcaLabelMultiLayerConduit parallelParams pcaMatrixes = do
  xs' <- CL.take (batchSize parallelParams)
  unless
    (L.null xs')
    (do let ys' =
              parMapChunk
                parallelParams
                rdeepseq
                (second $
                 L.zipWith
                   (\pcaMatrix x' ->
                       pcaTransform
                         (listArray (1, VU.length . L.head $ x') .
                          L.map LA.fromList . L.transpose . L.map VU.toList $
                          x')
                         pcaMatrix)
                   pcaMatrixes)
                xs'
        sourceList .
          L.map
            (second $
             L.map (L.map VU.fromList . L.transpose . L.map LA.toList . elems)) $
          ys'
        pcaLabelMultiLayerConduit parallelParams pcaMatrixes)




hPutMatrix :: Handle -> Matrix Double -> IO ()
hPutMatrix h mat = do
  BL.hPut h (encode len)
  BL.hPut h bs
  where
    !bs = encode . LA.toLists $ mat
    !len = fromIntegral $ BL.length bs :: Word32
    
readMatrixes :: FilePath -> IO [Matrix Double]
readMatrixes filePath = withBinaryFile filePath ReadMode go
  where
    go handle = do
      lenbs <- BL.hGet handle 4
      if BL.length lenbs < 4
        then return []
        else do
          let !len = fromIntegral (decode lenbs :: Word32) :: Int
          bs <- BL.hGet handle len
          mats <- go handle
          return $! (LA.fromLists . decode $ bs) : mats

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
  \handle -> do
    lenbs <- BL.hGet handle 4
    let !len = fromIntegral (decode lenbs :: Word32) :: Int
    bs <- BL.hGet handle len
    return . LA.fromLists . decode $ bs 

