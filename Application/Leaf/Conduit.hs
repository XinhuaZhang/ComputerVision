{-# LANGUAGE BangPatterns #-}
module Application.Leaf.Conduit where

import           Application.Leaf.Pooling
import           Classifier.LibLinear
import           Classifier.LibSVM
import           Control.Arrow
import           Control.Monad                as M
import           Control.Monad.IO.Class
import           Control.Monad.Parallel       as MP
import           Control.Monad.Trans.Resource
import           CV.Statistics.KMeans
import           CV.Statistics.PCA
import           CV.Utility.Parallel
import           CV.Utility.Time
-- import           CV.V4FilterConvolution
import           CV.Array.LabeledArray
import           CV.Filter.GaussianFilter
import           CV.Filter.PinwheelWavelet
import           CV.Utility.FFT
import           CV.Utility.RepaArrayUtility  (arrayToUnboxed, downsample)
import           Data.Array.Repa              as R
import           Data.Binary
import           Data.ByteString              as B
import           Data.ByteString.Lazy         as BL
import           Data.Complex
import           Data.Conduit
import           Data.Conduit.List            as CL
import           Data.List                    as L
import           Data.Vector                  as V
import           Data.Vector.Storable         as VS
import           Data.Vector.Unboxed          as VU
import           Data.Vector          as V
import           Foreign.Marshal.Array
import           Foreign.Ptr
import CV.Filter.PinwheelRing
import CV.Image
import Application.SymmetryDection.SymmetryDetection


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

featurePtrConduitP :: ParallelParams -> Conduit (a, VU.Vector Double) (ResourceT IO) (a, Ptr C'feature_node)
featurePtrConduitP parallelParams = do
  xs <- CL.take (batchSize parallelParams)
  unless
    (L.null xs)
    (do ys <-
          liftIO $
          MP.mapM
            (\(label, vec) -> do
               featurePtr <-
                 liftIO $ newArray . getFeature . Dense . VU.toList $ vec
               return (label, featurePtr))
            xs
        sourceList ys
        featurePtrConduitP parallelParams)


featureConduitP
  :: ParallelParams
  -> Conduit (a,VU.Vector Double) (IO) (a, [C'feature_node])
featureConduitP parallelParams = do
  xs <- CL.take (batchSize parallelParams)
  unless
    (L.null xs)
    (do let ys =
              parMapChunk
                parallelParams
                rseq
                (second $ getFeature . Dense . VU.toList)
                xs
        CL.sourceList ys
        featureConduitP parallelParams)


featureConduit :: Conduit (a, VU.Vector Double) (ResourceT IO) (a, [C'feature_node])
featureConduit = awaitForever (yield . second (getFeature . Dense . VU.toList))


-- {-# INLINE getOrientationHistogram #-}

-- getOrientationHistogram :: Int
--                         -> Int
--                         -> Int
--                         -> V4SeparableFilteredImageConvolution
--                         -> [[VU.Vector Double]]
-- getOrientationHistogram patchSize stride n (FourierMellinTransformFilteredImageConvolution (rows, cols) _ vecs) =
--   L.map (getDenseFeatures patchSize stride n . vector2Array (rows, cols)) .
--   L.concatMap L.concat $
--   vecs
-- getOrientationHistogram patchSize stride n (V4PolarSeparableFilteredImageConvolutionAxis (rows, cols) _ vecs) =
--   L.map (getDenseFeatures patchSize stride n . vector2Array (rows, cols)) . L.concat $
--   vecs

-- orientationHistogramConduit
--   :: ParallelParams
--   -> Int
--   -> Int
--   -> Int
--   -> Conduit (Double, [V4SeparableFilteredImageConvolution]) (ResourceT IO) (Double, [VU.Vector Double])
-- orientationHistogramConduit parallelParams patchSize stride n = do
--   xs <- CL.take (batchSize parallelParams)
--   unless
--     (L.null xs)
--     (do let ys =
--               parMapChunk
--                 parallelParams
--                 rdeepseq
--                 (\(label, filteredImages) ->
--                     let y = L.map (getOrientationHistogram patchSize stride n) filteredImages
--                         z =
--                           L.map VU.concat .
--                           L.transpose . L.map (L.map VU.concat . L.transpose) $
--                           y
--                     in (label, z))
--                 xs
--         sourceList ys
--         orientationHistogramConduit parallelParams patchSize stride n)


-- magnitudeConduit
--   :: ParallelParams
--   -> Conduit (Double, [V4SeparableFilteredImageConvolution]) (ResourceT IO) (Double, [VU.Vector Double])
-- magnitudeConduit parallelParams = do
--   xs <- CL.take (batchSize parallelParams)
--   unless
--     (L.null xs)
--     (do let ys =
--               parMapChunk
--                 parallelParams
--                 rdeepseq
--                 (\(label, filteredImages) ->
--                     let y =
--                           L.concatMap
--                             (\img ->
--                                 case img of
--                                   V4PolarSeparableFilteredImageConvolutionAxis _ _ vecs ->
--                                     L.concat vecs
--                                   FourierMellinTransformFilteredImageConvolution _ _ vecs ->
--                                     L.concatMap L.concat vecs)
--                             filteredImages
--                         !z =
--                           L.map VU.fromList .
--                           L.transpose . L.map (VU.toList . VU.map magnitude) $
--                           y
--                     in (label, z))
--                 xs
--         liftIO printCurrentTime
--         sourceList ys
--         magnitudeConduit parallelParams)


pcaSink
  :: ParallelParams
  -> FilePath
  -> Int
  -> Int
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
              parMap
                rdeepseq
                (second (L.map (pcaReduction pcaMat)))
                xs
        sourceList ys
        pcaConduit parallelParams pcaMat)


kmeansConduit
  :: ParallelParams
  -> KMeansModel
  -> Conduit (Double, [VU.Vector Double]) (ResourceT IO) (Double, VU.Vector Double)
kmeansConduit parallelParams model = do
  xs <- CL.take (batchSize parallelParams)
  unless
    (L.null xs)
    (do let ys =
              parMapChunk
                parallelParams
                rdeepseq
                (second $ vlad (center model))
                xs
        sourceList ys
        kmeansConduit parallelParams model)


writeMagnitudeConduit :: Conduit (Double, [VU.Vector Double]) (ResourceT IO) B.ByteString
writeMagnitudeConduit =
  awaitForever
    (\x ->
        let y = toStrict . encode . second (L.map VU.toList) $ x
        in yield y)

readMagnitudeConduit :: Conduit B.ByteString (ResourceT IO) (Double, [VU.Vector Double])
readMagnitudeConduit = awaitForever (\x -> undefined)


filterConduit
  :: (FilterConvolution a)
  => ParallelParams
  -> FFTW
  -> [a]
  -> GaussianFilterConvolution
  -> Bool
  -> Int
  -> Conduit (LabeledArray DIM3 Double) (ResourceT IO) (Double, [VU.Vector Double])
filterConduit parallelParams fftw filters gFilter gaussianFlag stride = do
  xs <- CL.take (batchSize parallelParams)
  unless
    (L.null xs)
    (do let (Z :. _ :. rows :. cols) =
              (\(LabeledArray _ x) -> extent x) . L.head $ xs
        ys <-
          M.mapM
            (\(LabeledArray label x) -> do
               let imgVecs =
                     L.map (VU.convert . VU.map (:+ 0)) . arrayToUnboxed $ x
                   filterList = L.concatMap getFilterConvolutionList filters
                   gFilterList = getFilterConvolutionList gFilter
               filteredImages <-
                 liftIO $
                 if gaussianFlag
                   then L.concat <$>
                        M.mapM
                          (\filter' ->
                              L.concat <$>
                              M.mapM
                                (\gFilter' ->
                                    M.mapM
                                      (\imgVec ->
                                          idft2d fftw rows cols $
                                          VS.zipWith3
                                            (\a b c' -> a * b * c')
                                            imgVec
                                            filter'
                                            gFilter')
                                      imgVecs)
                                gFilterList)
                          filterList
                   else L.concat <$>
                        M.mapM
                          (\filter' -> applyFilterConvolution fftw filter' imgVecs)
                          filters
               return (fromIntegral label, filteredImages))
            xs
        let zs =
              parMapChunk
                parallelParams
                rdeepseq
                (second $
                 L.map VU.fromList .
                 L.transpose .
                 if stride == 1
                   then L.map (VS.toList . VS.map magnitude)
                   else L.map
                          (R.toList .
                           R.map magnitude .
                           downsample [stride, stride] .
                           fromUnboxed (Z :. rows :. cols) . VS.convert))
                ys
        sourceList zs
        filterConduit parallelParams fftw filters gFilter gaussianFlag stride)


eigCovConduit
  :: (NFData a)
  => ParallelParams
  -> Int
  -> Conduit (a, [VU.Vector Double]) (ResourceT IO) (a, VU.Vector Double)
eigCovConduit parallelParams numPrincipal =
  awaitForever
    (\(label', vecs) ->
        let (PCAMatrix _ mat, _, _) = pcaSVD parallelParams numPrincipal vecs
        in yield (label', normalizeVec . VU.concat . V.toList $ mat))
  where normalizeVec vec
          | s == 0 = VU.replicate (VU.length vec) 0
          | otherwise = VU.map (/ s) vec
          where
            s = sqrt . VU.sum . VU.map (^ (2 :: Int)) $ vec
-- xs <- CL.take (batchSize parallelParams)
-- unless
--   (L.null xs)
--   (do let ys =
--             parMapChunk
--               parallelParams
--               rdeepseq
--               (\(label', vecs) ->
--                   let (PCAMatrix _ mat, _, _) = pcaSVDS numPrincipal vecs
--                   in (label',VU.concat . V.toList $ mat))
--               xs
--       sourceList ys
--       eigCovConduit parallelParams numPrincipal)



pinwheelRingGaussianConvolutionConduit
  :: ParallelParams
  -> FFTW
  -> PinwheelRingConvolution
  -> GaussianFilterConvolution1D
  -> Int
  -> Conduit (LabeledArray DIM3 Double) (ResourceT IO) (Double, [VU.Vector Double])
pinwheelRingGaussianConvolutionConduit parallelParams fftw pFilter gFilter stride = do
  xs <- CL.take (batchSize parallelParams)
  unless
    (L.null xs)
    (do ys <-
          liftIO $
          M.mapM
            (\(LabeledArray label x) -> do
               let imgVecs =
                     L.map (VU.convert . VU.map (:+ 0)) . arrayToUnboxed $ x
               zs <-
                 applyPinwheelRingConvolutionGaussian
                   fftw
                   pFilter
                   gFilter
                   stride
                   imgVecs
               return
                 (fromIntegral label, L.map (VU.map magnitude . VS.convert) zs))
            xs
        sourceList ys
        pinwheelRingGaussianConvolutionConduit
          parallelParams
          fftw
          pFilter
          gFilter
          stride)


filterConduit'
  :: (FilterConvolution a)
  => ParallelParams
  -> FFTW
  -> [a]
  -> GaussianFilterConvolution
  -> Bool
  -> Int
  -> Conduit (Double, ImageRepa) (ResourceT IO) (Double, [VU.Vector Double])
filterConduit' parallelParams fftw filters gFilter gaussianFlag stride = do
  xs <- CL.take (batchSize parallelParams)
  unless
    (L.null xs)
    (do let (Z :. _ :. rows :. cols) = extent . imageContent . snd . L.head $ xs
        ys <-
          M.mapM
            (\(label, Image _ x) -> do
               let imgVecs =
                     L.map (VU.convert . VU.map (:+ 0)) . arrayToUnboxed $ x
                   filterList = L.concatMap getFilterConvolutionList filters
                   gFilterList = getFilterConvolutionList gFilter
               filteredImages <-
                 liftIO $
                 if gaussianFlag
                   then L.concat <$>
                        M.mapM
                          (\filter' ->
                              L.concat <$>
                              M.mapM
                                (\gFilter' ->
                                    M.mapM
                                      (\imgVec ->
                                          idft2d fftw rows cols $
                                          VS.zipWith3
                                            (\a b c' -> a * b * c')
                                            imgVec
                                            filter'
                                            gFilter')
                                      imgVecs)
                                gFilterList)
                          filterList
                   else L.concat <$>
                        M.mapM
                          (\filter' -> applyFilterConvolution fftw filter' imgVecs)
                          filters
               return (label, filteredImages))
            xs
        let zs =
              parMapChunk
                parallelParams
                rdeepseq
                (second $
                 L.map VU.fromList .
                 L.transpose .
                 if stride == 1
                   then L.map (VS.toList . VS.map magnitude)
                   else L.map
                          (R.toList .
                           R.map magnitude .
                           downsample [stride, stride] .
                           fromUnboxed (Z :. rows :. cols) . VS.convert))
                ys
        sourceList zs
        filterConduit' parallelParams fftw filters gFilter gaussianFlag stride)


filterKeypointConduit
  :: ParallelParams
  -> FFTW
  -> PinwheelWaveletConvolution
  -> Int
  -> Double
  -> Conduit (LabeledArray DIM3 Double) (ResourceT IO) (Double, [VU.Vector Double])
filterKeypointConduit parallelParams fftw filters numPoints threshold = do
  xs <- CL.take (batchSize parallelParams)
  unless
    (L.null xs)
    (do ys <-
          liftIO $
          M.mapM
            (\(LabeledArray label x) -> do
               let imgVecs =
                     L.map (VU.convert . VU.map (:+ 0)) . arrayToUnboxed $ x
               zs <- applyPinwheelWaveletConvolution fftw filters imgVecs
               return (fromIntegral label, zs))
            xs
        let labelKeypoints =
              parMapChunk
                parallelParams
                rdeepseq
                (\(label, filteredImages) ->
                    let normalizedFilteredImages =
                          L.map
                            (L.map
                               (L.map
                                  (VS.map
                                     (\x ->
                                         if magnitude x < 10 ** (-6)
                                           then 0 :+ 0
                                           else x))))
                            filteredImages
                        rotation =
                          L.map
                            (L.map rotationSymmetryDegree .
                             L.transpose . L.map VS.toList) .
                          rotationSymmetry
                            (pinwheelWaveletAngularFreqs . getFilterParams $ filters)
                            numPoints $
                          normalizedFilteredImages
                        reflection =
                          L.map
                            (L.map reflectionSymmetryDegree .
                             L.transpose . L.map VS.toList) .
                          reflectionSymmetry
                            (pinwheelWaveletAngularFreqs . getFilterParams $ filters)
                            numPoints $
                          normalizedFilteredImages
                        degree = L.concat $ L.zipWith (L.zipWith (+)) rotation reflection
                        points =
                          findKeypoint threshold .
                          R.fromListUnboxed
                            (Z :.
                             (L.length . pinwheelWaveletRadialScale . getFilterParams $
                              filters) :.
                             rows :.
                             cols) $
                          degree
                        filteredImagesVec = V.fromList filteredImages
                        keypoints =
                          L.map
                            (\(Keypoint s idx _) ->
                                VU.fromList .
                                L.map (\a -> magnitude $ a VS.! idx) . L.concat $
                                (filteredImagesVec V.! s))
                            points
                    in (label, keypoints))
                ys
        sourceList labelKeypoints
        filterKeypointConduit parallelParams fftw filters numPoints threshold)
  where
    rows = pinwheelWaveletRows . getFilterParams $ filters
    cols = pinwheelWaveletCols . getFilterParams $ filters


filterExpansionConduit
  :: (FilterExpansion a)
  => ParallelParams
  -> a
  -> Conduit (LabeledArray DIM3 Double) (ResourceT IO) (Double, VU.Vector Double)
filterExpansionConduit parallelParams filter = do
  xs <- CL.take (batchSize parallelParams)
  unless
    (L.null xs)
    (do let ys =
              parMapChunk
                parallelParams
                rdeepseq
                (\(LabeledArray label x) ->
                   let imgVecs =
                         L.map (VU.convert . VU.map (:+ 0)) . arrayToUnboxed $ x
                   in ( fromIntegral label
                      , normalizeVec .
                        VU.map magnitude .
                        VU.fromList . applyFilterExpansion filter $
                        imgVecs))
                xs
        sourceList ys
        filterExpansionConduit parallelParams filter)
  where
    normalizeVec vec
      | s == 0 = VU.replicate (VU.length vec) 0
      | otherwise = VU.map (/ s) vec
      where
        s = sqrt . VU.sum . VU.map (^ (2 :: Int)) $ vec
