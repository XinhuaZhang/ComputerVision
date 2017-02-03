{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Application.PinwheelPCANet.PCA where

import           Control.Arrow
import           Control.Monad                  as M
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           CV.Array.LabeledArray
import           CV.Filter.GaussianFilter       as G
import           CV.Filter.PolarSeparableFilter
import           CV.Utility.Parallel
import           CV.Utility.RepaArrayUtility
import           Data.Array
import           Data.Array.Repa                as R
import           Data.Binary
import           Data.ByteString.Lazy           as BL
import           Data.Complex                   as C
import           Data.Conduit
import           Data.Conduit.List              as CL
import           Data.List                      as L
import           Data.Vector.Unboxed            as VU
import           Numeric.LinearAlgebra.Data     as LA
import           Numeric.Statistics.PCA
import           System.IO

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
  -> Conduit (R.Array s DIM3 Double) (ResourceT IO) (R.Array U DIM3 Double)
pcaConduit parallelParams pcaMatrix =
  do xs <- CL.take (batchSize parallelParams)
     unless (L.null xs)
            (do let ys =
                      parMapChunk parallelParams
                                  rseq
                                  (pcaTransformArray pcaMatrix)
                                  xs
                sourceList ys
                pcaConduit parallelParams pcaMatrix)

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
  :: ParallelParams
  -> [PolarSeparableFilterParamsSet]
  -> [GaussianFilterParams]
  -> [Int]
  -> [Matrix Double]
  -> Conduit (LabeledArray DIM3 Double) (ResourceT IO) (Int, [[VU.Vector Double]])
pinwheelPCANetVariedSizeConduit parallelParams filterParamsList gaussianFilterParamsList factors pcaMatrix = do
  xs <- CL.take (batchSize parallelParams)
  unless
    (L.null xs)
    (do let ys =
              parMapChunk
                parallelParams
                rseq
                (\(LabeledArray label arr') ->
                    ( label
                    , pinwheelPCANetVariedSize
                        filterParamsList
                        gaussianFilterParamsList
                        factors
                        pcaMatrix
                        arr'))
                xs
        sourceList ys
        pinwheelPCANetVariedSizeConduit
          parallelParams
          filterParamsList
          gaussianFilterParamsList
          factors
          pcaMatrix)

{-# INLINE pinwheelPCANetVariedSize #-}

pinwheelPCANetVariedSize
  :: (R.Source s Double)
  => [PolarSeparableFilterParamsSet]
  -> [GaussianFilterParams]
  -> [Int]
  -> [Matrix Double]
  -> R.Array s DIM3 Double
  -> [[VU.Vector Double]]
pinwheelPCANetVariedSize filterParamsList gaussianFilterParamsList factors pcaMatrix inputArr =
  L.zipWith3
    (\gaussianFilterParams factor arr' ->
        let gArr = G.applyFilterVariedSize gaussianFilterParams arr'
            downsampledArr =
              if factor == 1
                then gArr
                else downsample [factor, factor, 1] gArr
        in extractPointwiseFeature downsampledArr)
    gaussianFilterParamsList
    factors .
  L.tail .
  L.scanl'
    (\arr' (filterParams, pcaMat) ->
        pcaTransformArray pcaMat .
        R.map C.magnitude . applyFilterSetVariedSize filterParams . R.map (:+ 0) $
        arr')
    (computeS . delay $ inputArr) $
  L.zip filterParamsList pcaMatrix
  

pinwheelPCANetTopVariedSizeConduit
  :: ParallelParams
  -> [PolarSeparableFilterParamsSet]
  -> GaussianFilterParams
  -> Int
  -> [Matrix Double]
  -> Conduit (LabeledArray DIM3 Double) (ResourceT IO) (Int, [VU.Vector Double])
pinwheelPCANetTopVariedSizeConduit parallelParams filterParamsList gaussianFilterParams factor pcaMatrix = do
  xs <- CL.take (batchSize parallelParams)
  unless
    (L.null xs)
    (do let ys =
              parMapChunk
                parallelParams
                rseq
                (\(LabeledArray label arr') ->
                    ( label
                    , pinwheelPCANetTopVariedSize
                        filterParamsList
                        gaussianFilterParams
                        factor
                        pcaMatrix
                        arr'))
                xs
        sourceList ys
        pinwheelPCANetTopVariedSizeConduit
          parallelParams
          filterParamsList
          gaussianFilterParams
          factor
          pcaMatrix)

{-# INLINE pinwheelPCANetTopVariedSize #-}

pinwheelPCANetTopVariedSize
  :: (R.Source s Double)
  => [PolarSeparableFilterParamsSet]
  -> GaussianFilterParams
  -> Int
  -> [Matrix Double]
  -> R.Array s DIM3 Double
  -> [VU.Vector Double]
pinwheelPCANetTopVariedSize filterParamsList gaussianFilterParams factor pcaMatrix inputArr =
  extractPointwiseFeature downsampledArr
  where
    top =
      L.foldl'
        (\arr' (filterParams, pcaMat) ->
            pcaTransformArray pcaMat .
            R.map C.magnitude .
            applyFilterSetVariedSize filterParams . R.map (:+ 0) $
            arr')
        (computeS . delay $ inputArr) $
      L.zip filterParamsList pcaMatrix
    gArr = G.applyFilterVariedSize gaussianFilterParams top
    downsampledArr =
      if factor == 1
        then gArr
        else downsample [factor, factor, 1] gArr


-- {-# INLINE pinwheelPCANetComplexVariedSize #-}

-- pinwheelPCANetComplexVariedSize
--   :: (R.Source s Double)
--   => [PolarSeparableFilterParamsSet]
--   -> [Int]
--   -> [Matrix Double]
--   -> R.Array s DIM3 Double
--   -> [[VU.Vector Double]]
-- pinwheelPCANetComplexVariedSize filterParamsList factors pcaMatrix inputArr =
--   L.zipWith
--     (\factor arr' ->
--         let downsampledArr =
--               if factor == 1
--                 then arr'
--                 else computeUnboxedS $ downsample [factor, factor, 1] arr'
--         in extractPointwiseFeature downsampledArr)
--     factors .
--   L.tail .
--   L.scanl'
--     (\arr' (filterParams, pcaMat) ->
--         pcaTransformArray pcaMat .
--         R.map C.magnitude . applyFilterSetVariedSize filterParams $
--         arr')
--     (R.map (:+ 0) inputArr) $
--   L.zip filterParamsList pcaMatrix
