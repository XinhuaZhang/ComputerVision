module Application.ECCV2018.Conduit where

import           Classifier.LibLinear
import           Control.Arrow
import           Control.Monad                as M
import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Trans.Resource
import           CV.Array.LabeledArray
import           CV.Statistics.KMeans
import           CV.Statistics.PCA
import           CV.Utility.Parallel
import           CV.Utility.RepaArrayUtility
import           CV.Utility.Utility
import           Data.Array.Repa              as R
import           Data.Binary
import           Data.Complex
import           Data.Conduit
import           Data.Conduit.List            as CL
import           Data.List                    as L
import           Data.Vector.Unboxed          as VU
import           Foreign.Marshal.Array
import           Foreign.Ptr

-- input list: layer, free degrees
-- output list: layer, free degrees
invariantFeatureExtractionConduit
  :: ParallelParams
  -> Int
  -> Conduit (Double, [[[R.Array U DIM3 Double]]]) (ResourceT IO) (Double, [[[VU.Vector Double]]])
invariantFeatureExtractionConduit parallelParams stride = do
  xs <- CL.take (batchSize parallelParams)
  unless
    (L.null xs)
    (do let ys =
              parMapChunk
                parallelParams
                rdeepseq
                (\(label, y) ->
                   let zs =
                         L.map
                           (L.map
                              (L.concatMap
                                 (\arr ->
                                    let downsampledArr =
                                          downsample [stride, stride, 1] arr
                                        (Z :. _ :. rows :. cols) =
                                          extent downsampledArr
                                        as =
                                          [ l2norm .
                                          toUnboxed .
                                          computeS . R.slice downsampledArr $
                                          (Z :. All :. i :. j)
                                          | i <- [0 .. rows - 1]
                                          , j <- [0 .. cols - 1]
                                          ]
                                        -- m =
                                        --   (L.sum . L.map fst $ as) /
                                        --   (fromIntegral . L.length $ as)
                                       -- L.map snd . L.filter (\(a, b) -> a > m) $
                                       -- as
                                    in as)))
                           y
                   in (label, zs))
                xs
        sourceList ys
        invariantFeatureExtractionConduit parallelParams stride)
        

objectFeatureExtractionConduit
  :: ParallelParams
  -> Int
  -> Conduit (LabeledArray DIM3 Double, (Double, [[[R.Array U DIM3 Double]]])) (ResourceT IO) (Double, [[[VU.Vector Double]]])
objectFeatureExtractionConduit parallelParams stride = do
  xs <- CL.take (batchSize parallelParams)
  unless
    (L.null xs)
    (do let ys =
              parMapChunk
                parallelParams
                rdeepseq
                (\(LabeledArray _ imgArr, (label, y)) ->
                   let zs =
                         L.map
                           (L.map
                              (L.concatMap
                                 (\arr ->
                                    let downsampledArr =
                                          downsample [stride, stride, 1] arr
                                        downsampledImg =
                                          downsample [stride, stride, 1] imgArr
                                        (Z :. _ :. rows :. cols) =
                                          extent downsampledArr
                                        idx =
                                          L.map fst .
                                          L.filter (\(_, b) -> b > 0) $
                                          [ ( (i, j)
                                            , L.sum .
                                              R.toList . R.slice downsampledImg $
                                              (Z :. All :. i :. j))
                                          | i <- [0 .. rows - 1]
                                          , j <- [0 .. cols - 1]
                                          ]
                                    in L.map
                                         (\(i, j) ->
                                            l2norm .
                                            toUnboxed .
                                            computeS . R.slice downsampledArr $
                                            (Z :. All :. i :. j))
                                         idx)))
                           y
                   in (label, zs))
                xs
        sourceList ys
        objectFeatureExtractionConduit parallelParams stride)

-- input list: layer, free degrees
-- output list: layer, free degrees
nonInvariantFeatureExtractionConduit
  :: ParallelParams
  -> Int
  -> Conduit [[LabeledArray DIM3 Double]] (ResourceT IO) (Double, [[VU.Vector Double]])
nonInvariantFeatureExtractionConduit parallelParams stride = do
  xs <- CL.take (batchSize parallelParams)
  unless
    (L.null xs)
    (do let ys =
              parMapChunk
                parallelParams
                rdeepseq
                (\y ->
                   let label =
                         (\(LabeledArray label _) -> label) . L.head . L.head $
                         y
                       zs =
                         L.map
                           (L.map (-- rescaleUnboxedVector (0, 1)
                                   l2norm .
                                   VU.concat) .
                            L.transpose .
                            L.map
                              (\(LabeledArray _ arr) ->
                                 let downsampledArr =
                                       downsample [stride, stride, 1] arr
                                     (Z :. _ :. rows :. cols) =
                                       extent downsampledArr
                                 in [ toUnboxed .
                                    computeS . R.slice downsampledArr $
                                    (Z :. All :. i :. j)
                                    | i <- generateCenters rows (div rows 2 - 1) -- generateCenters rows (div rows 2 - 1) -- [0 .. rows - 1]
                                    , j <- generateCenters rows (div cols 2 - 1)  -- generateCenters cols (div cols 2 - 1) -- [0 .. cols - 1]
                                    ]))
                           y
                   in (fromIntegral label, zs))
                xs
        sourceList ys
        nonInvariantFeatureExtractionConduit parallelParams stride)

-- from outter-most to inner-most:
-- layer, filterType, free degrees
kmeansConduit
  :: ParallelParams
  -> [[KMeansModel]]
  -> Conduit (Double, [[[VU.Vector Double]]]) (ResourceT IO) (Double, VU.Vector Double)
kmeansConduit parallelParams models = do
  xs <- CL.take (batchSize parallelParams)
  unless
    (L.null xs)
    (do let ys =
              parMapChunk
                parallelParams
                rdeepseq
                (second $
                 VU.concat .
                 L.zipWith
                   (\model zs -> VU.concat $ L.zipWith (vlad . center) model zs)
                   models)
                xs
        sourceList ys
        kmeansConduit parallelParams models)

-- from outter-most to inner-most:
-- layer, filterType,  free degrees
kmeansSink
  :: ParallelParams
  -> Int
  -> Int
  -> FilePath
  -> Double
  -> Sink (Double, [[[VU.Vector Double]]]) (ResourceT IO) [[KMeansModel]]
kmeansSink parallelParams numExample numGaussian kmeansFile threshold = do
  xs <-
    if numExample > 10000
      then CL.take 10000
      else CL.take numExample
  let ys =
        L.map (L.map L.concat . L.transpose) . L.transpose . snd . L.unzip $ xs
  model <-
    liftIO .
    M.mapM (M.mapM (kmeans parallelParams numGaussian kmeansFile threshold)) $
    ys
  liftIO $ encodeFile kmeansFile model
  return model

featurePtrConduit :: Conduit (a, VU.Vector Double) (ResourceT IO) (a, Ptr C'feature_node)
featurePtrConduit =
  awaitForever
    (\(label, vec) -> do
       featurePtr <- liftIO $ newArray . getFeature . Dense . VU.toList $ vec
       yield (label, featurePtr))

featureConduit :: Conduit (a, VU.Vector Double) (ResourceT IO) (a, [C'feature_node])
featureConduit = awaitForever (yield . second (getFeature . Dense . VU.toList))

{-# INLINE generateCenters #-}

generateCenters :: Int -> Int -> [Int]
generateCenters len n
  | len == n = [0 .. len - 1]
  | odd n = L.map (\x -> x * dist + center) [-m .. m]
  | otherwise = error "generateCenters: numGrid is even."
  -- | otherwise = L.map fromIntegral [stride,2 * stride .. n * stride]
  where
    stride = div len (n + 1)
    center = div len 2
    dist = 1
    m = div n 2

pcaSink
  :: ParallelParams
  -> FilePath
  -> Int
  -> Int
  -> Sink (Double, VU.Vector Double) (ResourceT IO) (PCAMatrix Double, [(Double, VU.Vector Double)])
pcaSink parallelParams filePath numPrincipal numTrain = do
  xs <- CL.take numTrain
  let (labels, ys) = L.unzip $ xs
      (pcaMat, _, _) = pcaSVD parallelParams numPrincipal ys
      vecs =
        parMapChunk parallelParams rdeepseq (pcaReduction pcaMat) .
        snd . L.unzip $
        xs
  liftIO $ encodeFile filePath pcaMat
  return (pcaMat, L.zip labels vecs)

pcaConduit
  :: ParallelParams
  -> PCAMatrix Double
  -> Conduit (Double, VU.Vector Double) (ResourceT IO) (Double, VU.Vector Double)
pcaConduit parallelParams pcaMat = do
  xs <- CL.take (batchSize parallelParams)
  unless
    (L.null xs)
    (do let ys =
              parMapChunk
                parallelParams
                rdeepseq
                (second (pcaReduction pcaMat))
                xs
        sourceList ys
        pcaConduit parallelParams pcaMat)
