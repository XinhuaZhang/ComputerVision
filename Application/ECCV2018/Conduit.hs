module Application.ECCV2018.Conduit where

import           Classifier.LibLinear
import           Control.Arrow
import           Control.Monad                as M
import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Trans.Resource
import           CV.Array.LabeledArray
import           CV.Statistics.KMeans
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
  -> Conduit [[LabeledArray DIM3 Double]] (ResourceT IO) (Double, [[VU.Vector Double]])
invariantFeatureExtractionConduit parallelParams stride = do
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
                           (L.concatMap
                              (\(LabeledArray _ arr) ->
                                 let downsampledArr =
                                       downsample [stride, stride, 1] arr
                                     (Z :. _ :. rows :. cols) =
                                       extent downsampledArr
                                 in [ l2norm .
                                    -- rescaleUnboxedVector (0, 1) .
                                    toUnboxed .
                                    computeS . R.slice downsampledArr $
                                    (Z :. All :. i :. j)
                                    | i <-
                                        generateCenters
                                          rows
                                          (round $ fromIntegral rows / 3 * 2) --  i <- generateCenters rows (div rows 2 - 1) -- [0 .. rows - 1]
                                    , j <-
                                        generateCenters
                                          cols
                                          (round $ fromIntegral cols / 3 * 2) -- j <- generateCenters cols (div cols 2 - 1) -- [0 .. cols - 1]
                                    ]))
                           y
                   in (fromIntegral label, zs))
                xs
        sourceList ys
        invariantFeatureExtractionConduit parallelParams stride)
        
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
  xs <- CL.take numExample
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
