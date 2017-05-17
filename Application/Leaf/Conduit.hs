module Application.Leaf.Conduit where

import           Application.Leaf.Pooling
import           Classifier.LibLinear
import           Classifier.LibSVM
import           Control.Arrow
import           Control.Monad                as M
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           CV.FilterExpansion
import           CV.Statistics.PCA
import           CV.Utility.Parallel
import           CV.V4FilterConvolution
import           Data.Array.Repa              as R
import           Data.Binary
import           Data.Complex
import           Data.Conduit
import           Data.Conduit.List            as CL
import           Data.List                    as L
import           Data.Vector                  as V
import           Data.Vector.Unboxed          as VU
import           Foreign.Marshal.Array
import           Foreign.Ptr


{-# INLINE complexDistance #-}

complexDistance :: VU.Vector (Complex Double)
                -> VU.Vector (Complex Double)
                -> Double
complexDistance vec1 vec2
  | s == 0 = 0
  | otherwise = magnitude $ VU.sum (VU.zipWith (\x y -> x * conjugate y) vec1 vec2) / (s :+ 0)
  where
    s = VU.sum $ VU.zipWith (\x y -> magnitude x * magnitude y) vec1 vec2


{-# INLINE complexDistanceEuclidean #-}

complexDistanceEuclidean :: VU.Vector (Complex Double)
                         -> VU.Vector (Complex Double)
                         -> Double
complexDistanceEuclidean vec1 vec2 =
  (-1) *
  (sqrt . VU.sum $ VU.zipWith (\a b -> magnitude (a - b) ^ (2 :: Int)) vec1 vec2)

complexKernelP :: [VU.Vector (Complex Double)] -> IO [[Double]]
complexKernelP xs = do
  arr <-
    computeUnboxedP $
    fromFunction (Z :. n :. n) $ \(Z :. j :. i) ->
      if j <= i
        then complexDistance (vec V.! i) (vec V.! j)
        else 0
  return
    [ [ if j <= i
      then arr R.! (Z :. j :. i)
      else arr R.! (Z :. i :. j)
    | i <- [0 .. n - 1]
    ]
    | j <- [0 .. n - 1]
    ]
  where
    n = L.length xs
    vec = V.fromList xs


complexKernel :: ParallelParams -> [VU.Vector (Complex Double)] -> [[Double]]
complexKernel parallelParams xs = do
  parMapChunk
    parallelParams
    rdeepseq
    (\j ->
       [ if j <= i
         then arr R.! (Z :. j :. i)
         else arr R.! (Z :. i :. j)
       | i <- [0 .. n - 1]
       ])
    [0 .. n - 1]
  where
    n = L.length xs
    vec = V.fromList xs
    arr =
      fromFunction (Z :. n :. n) $ \(Z :. j :. i) ->
        if j <= i
          then complexDistanceEuclidean (vec V.! i) (vec V.! j)
          else 0


libSVMPredictConduit
  :: ParallelParams
  -> [VU.Vector (Complex Double)]
  -> Conduit (Double, VU.Vector (Complex Double)) (ResourceT IO) (Double, Ptr C'svm_node)
libSVMPredictConduit parallelParams trainFeatures = do
  xs <- CL.take (batchSize parallelParams)
  unless
    (L.null xs)
    (do let (labels, ys) = L.unzip xs
            zs =
              parMapChunk
                parallelParams
                rdeepseq
                (\y -> L.map (complexDistanceEuclidean y) trainFeatures)
                ys
        ptrs <- liftIO $ M.mapM (getPreComputedKernelFeatureVecPtr (-1)) zs
        sourceList $ L.zip labels ptrs
        libSVMPredictConduit parallelParams trainFeatures)


featurePtrConduit :: Conduit (a, VU.Vector Double) (ResourceT IO) (a, Ptr C'feature_node)
featurePtrConduit =
  awaitForever
    (\(label, vec) -> do
       featurePtr <- liftIO $ newArray . getFeature . Dense . VU.toList $ vec
       yield (label, featurePtr))


featureConduitP
  :: ParallelParams
  -> Conduit (a,VU.Vector Double) (ResourceT IO) (a, [C'feature_node])
featureConduitP parallelParams = do
  xs <- CL.take (batchSize parallelParams)
  unless
    (L.null xs)
    (do let ys =
              parMapChunk
                parallelParams
                rseq
                (second $ getFeature . Dense . VU.toList) $
              xs
        CL.sourceList ys
        featureConduitP parallelParams)


featureConduit :: Conduit (a, VU.Vector Double) (ResourceT IO) (a, [C'feature_node])
featureConduit = awaitForever (yield . second (getFeature . Dense . VU.toList))


{-# INLINE getOrientationHistogram #-}

getOrientationHistogram :: Int
                        -> Int
                        -> Int
                        -> V4SeparableFilteredImageConvolution
                        -> [[VU.Vector Double]]
getOrientationHistogram patchSize stride n (FourierMellinTransformFilteredImageConvolution (rows, cols) _ vecs) =
  L.map (getDenseFeatures patchSize stride n . vector2Array (rows, cols)) .
  L.concatMap L.concat $
  vecs
getOrientationHistogram patchSize stride n (V4PolarSeparableFilteredImageConvolutionAxis (rows, cols) _ vecs) =
  L.map (getDenseFeatures patchSize stride n . vector2Array (rows, cols)) . L.concat $
  vecs

orientationHistogramConduit
  :: ParallelParams
  -> Int
  -> Int
  -> Int
  -> Conduit (Double, [V4SeparableFilteredImageConvolution]) (ResourceT IO) (Double, [VU.Vector Double])
orientationHistogramConduit parallelParams patchSize stride n = do
  xs <- CL.take (batchSize parallelParams)
  unless
    (L.null xs)
    (do let ys =
              parMapChunk
                parallelParams
                rdeepseq
                (\(label, filteredImages) ->
                    let y = L.map (getOrientationHistogram patchSize stride n) filteredImages
                        z =
                          L.map VU.concat .
                          L.transpose . L.map (L.map VU.concat . L.transpose) $
                          y
                    in (label, z))
                xs
        sourceList ys
        orientationHistogramConduit parallelParams patchSize stride n)


pcaSink
  :: ParallelParams
  -> FilePath
  -> Int -> Int
  -> Sink (Double, [VU.Vector Double]) (ResourceT IO) (PCAMatrix Double, [[VU.Vector Double]])
pcaSink parallelParams filePath numPrincipal numTrain = do
  xs <- CL.take numTrain
  let ys = L.concat . snd . L.unzip $ xs
      (pcaMat, _, _) = pcaSVD parallelParams numPrincipal ys
      vecs =
        parMapChunk parallelParams rdeepseq (L.map (pcaReduction pcaMat)) .
        snd . L.unzip $
        xs
  liftIO $ encodeFile filePath pcaMat
  return (pcaMat, vecs)

pcaConduit
  :: ParallelParams
  -> PCAMatrix Double
  -> Conduit (Double, [VU.Vector Double]) (ResourceT IO) (Double, [VU.Vector Double])
pcaConduit parallelParams pcaMat = do
  xs <- CL.take (batchSize parallelParams)
  unless
    (L.null xs)
    (do let ys =
              parMapChunk
                parallelParams
                rdeepseq
                (second (L.map (pcaReduction pcaMat)))
                xs
        sourceList ys
        pcaConduit parallelParams pcaMat)
