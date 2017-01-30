{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

module Application.PinwheelPCANet.PCA where

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
import           CV.Filter.PolarSeparableFilter
import           Data.Complex                   as C

hPCASink
  :: ParallelParams
  -> Handle
  -> Int
  -> Int
  -> Int
  -> Sink (R.Array U DIM3 Double) (ResourceT IO) [R.Array U DIM3 Double]
hPCASink parallelParams h numExample numPrincipal downsampleFactor' = do
  arrs <- CL.take numExample
  if L.null arrs
    then error "pcaSink: input data is empty."
    else do
      let (Z :. nf' :. _ :. _) = extent . L.head $ arrs
          arr' =
            listArray (1, nf') .
            L.map LA.fromList .
            L.transpose .
            L.map VU.toList .
            L.concatMap
              (extractPointwiseFeature .
               downsample [downsampleFactor', downsampleFactor', 1]) $
            arrs
          pcaMatrix = pcaN arr' numPrincipal
          reducedArrs =
            parMapChunk parallelParams rseq (pcaTransformArray pcaMatrix) arrs
      liftIO $ hPutMatrix h pcaMatrix
      liftIO . System.IO.putStrLn $ "One layer is finished."
      return reducedArrs

{-# INLINE pcaTransformArray #-}

pcaTransformArray
  :: (R.Source s Double)
  => Matrix Double -> R.Array s DIM3 Double -> R.Array U DIM3 Double
pcaTransformArray pcaMatrix x' = deepSeqArray arr' arr'
  where
    (Z :. nf' :. ny' :. nx') = extent x'
    newNf = LA.cols pcaMatrix
    arr' =
      R.fromListUnboxed (Z :. newNf :. ny' :. nx') .
      L.concatMap LA.toList .
      elems .
      pcaTransform
        (listArray (1, nf') . L.map LA.fromList . extractFeatureMap $ x') $
      pcaMatrix

pcaConduit
  :: (R.Source s Double)
  => ParallelParams
  -> Matrix Double
  -> Int
  -> Conduit (R.Array s DIM3 Double) (ResourceT IO) (R.Array U DIM3 Double)
pcaConduit parallelParams pcaMatrix downsampleFactor' = do
  xs <- CL.take (batchSize parallelParams)
  unless
    (L.null xs)
    (do let ys =
              parMapChunk
                parallelParams
                rseq
                (\x' ->
                    let y' =
                          computeUnboxedS .
                          downsample [downsampleFactor', downsampleFactor', 1] .
                          pcaTransformArray pcaMatrix $
                          x'
                    in deepSeqArray y' y')
                xs
        sourceList ys
        pcaConduit parallelParams pcaMatrix downsampleFactor')

pcaLabelConduit
  :: (R.Source s Double)
  => ParallelParams
  -> Matrix Double
  -> Conduit (Int, R.Array s DIM3 Double) (ResourceT IO) (Int, R.Array U DIM3 Double)
pcaLabelConduit parallelParams pcaMatrix = do
  xs <- CL.take (batchSize parallelParams)
  unless
    (L.null xs)
    (do let !ys =
              parMapChunk
                parallelParams
                rseq
                (second $ pcaTransformArray pcaMatrix)
                xs
        sourceList ys
        pcaLabelConduit parallelParams pcaMatrix)

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


pinwheelPCANetVariedSizeConduit
  :: (R.Source s Double)
  => ParallelParams
  -> [PolarSeparableFilterParamsSet]
  -> [Int]
  -> [Matrix Double]
  -> Conduit (Int, R.Array s DIM3 Double) (ResourceT IO) (Int, [[VU.Vector Double]])
pinwheelPCANetVariedSizeConduit parallelParams filterParamsList factors pcaMatrix = do
  xs <- CL.take (batchSize parallelParams)
  unless
    (L.null xs)
    (do let ys =
              parMapChunk
                parallelParams
                rseq
                (second $ pinwheelPCANetVariedSize filterParamsList factors pcaMatrix)
                xs
        sourceList ys
        pinwheelPCANetVariedSizeConduit
          parallelParams
          filterParamsList
          factors
          pcaMatrix)

{-# INLINE pinwheelPCANetVariedSize #-}

pinwheelPCANetVariedSize
  :: (R.Source s Double)
  => [PolarSeparableFilterParamsSet]
  -> [Int]
  -> [Matrix Double]
  -> R.Array s DIM3 Double
  -> [[VU.Vector Double]]
pinwheelPCANetVariedSize filterParamsList factors pcaMatrix inputArr =
  L.zipWith
    (\factor arr' ->
        let downsampledArr =
              if factor == 1
                then arr'
                else computeUnboxedS $ downsample [factor, factor, 1] arr'
        in extractPointwiseFeature downsampledArr)
    factors .
  L.tail .
  L.scanl'
    (\arr' (filterParams, pcaMat) ->
        pcaTransformArray pcaMat .
        R.map C.magnitude . applyFilterSetVariedSize filterParams . R.map (:+ 0) $
        arr')
    (computeS . delay $ inputArr) $
  L.zip filterParamsList pcaMatrix
