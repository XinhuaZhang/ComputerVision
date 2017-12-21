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
import           Data.Array.Repa              as R
import           Data.Binary
import           Data.Complex
import           Data.Conduit
import           Data.Conduit.List            as CL
import           Data.List                    as L
import           Data.Vector.Unboxed          as VU
import           Foreign.Marshal.Array
import           Foreign.Ptr

-- input list: layer
-- output list: layer, position
trainFeatureConduit
  :: ParallelParams
  -> Int
  -> Conduit [LabeledArray DIM3 Double] (ResourceT IO) (Double, [[VU.Vector Double]])
trainFeatureConduit parallelParams stride = do
  xs <- CL.take (batchSize parallelParams)
  unless
    (L.null xs)
    (do let ys =
              parMapChunk
                parallelParams
                rdeepseq
                (\y ->
                   let label = (\(LabeledArray label _) -> label) . L.head $ y
                       zs =
                         L.map
                           (\(LabeledArray _ arr) ->
                              let downsampledArr =
                                    downsample [stride, stride, 1] arr
                                  (Z :. _ :. rows :. cols) =
                                    extent downsampledArr
                              in [ toUnboxed . computeS . R.slice downsampledArr $
                                 (Z :. All :. i :. j)
                                 | i <- [0 .. rows - 1]
                                 , j <- [0 .. cols - 1]
                                 ]) $
                         y
                   in (fromIntegral label, zs))
                xs
        sourceList ys
        trainFeatureConduit parallelParams stride)


-- input list: radius, layer
-- output list: radius, layer, position
predictFeatureConduit
  :: ParallelParams
  -> Int
  -> Conduit [[LabeledArray DIM3 Double]] (ResourceT IO) (Double, [[[VU.Vector Double]]])
predictFeatureConduit parallelParams stride = do
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
                           (L.map
                              (\(LabeledArray _ arr) ->
                                 let downsampledArr =
                                       downsample [stride, stride, 1] arr
                                     (Z :. _ :. rows :. cols) =
                                       extent downsampledArr
                                 in [ toUnboxed .
                                    computeS . R.slice downsampledArr $
                                    (Z :. All :. i :. j)
                                    | i <- [0 .. rows - 1]
                                    , j <- [0 .. cols - 1]
                                    ]))
                           y
                   in (fromIntegral label, zs))
                xs
        sourceList ys
        predictFeatureConduit parallelParams stride)

-- from outter-most to inner-most:
-- layer, position
kmeansConduit
  :: ParallelParams
  -> [KMeansModel]
  -> Conduit (Double, [[VU.Vector Double]]) (ResourceT IO) (Double, VU.Vector Double)
kmeansConduit parallelParams models = do
  xs <- CL.take (batchSize parallelParams)
  unless
    (L.null xs)
    (do let ys =
              parMapChunk
                parallelParams
                rdeepseq
                (second $ VU.concat . L.zipWith (vlad . center) models)
                xs
        sourceList ys
        kmeansConduit parallelParams models)

-- from outter-most to inner-most:
-- radisu,layer, position
kmeansPredictConduit
  :: ParallelParams
  -> [KMeansModel]
  -> Conduit (Double, [[[VU.Vector Double]]]) (ResourceT IO) (Double, [VU.Vector Double])
kmeansPredictConduit parallelParams models = do
  xs <- CL.take (batchSize parallelParams)
  unless
    (L.null xs)
    (do let ys =
              parMapChunk
                parallelParams
                rdeepseq
                (second $ L.map (VU.concat . L.zipWith (vlad . center) models))
                xs
        sourceList ys
        kmeansPredictConduit parallelParams models)

-- from outter-most to inner-most:
-- layer, position
kmeansSink
  :: ParallelParams
  -> Int
  -> Int
  -> FilePath
  -> Double
  -> Sink (Double, [[VU.Vector Double]]) (ResourceT IO) [KMeansModel]
kmeansSink parallelParams numExample numGaussian kmeansFile threshold = do
  xs <- CL.take numExample
  let ys = L.map L.concat . L.transpose . snd . L.unzip $ xs
  model <-
    liftIO . M.mapM (kmeans parallelParams numGaussian kmeansFile threshold) $
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

featurePredictConduit :: Conduit (a, [VU.Vector Double]) (ResourceT IO) (a, [[C'feature_node]])
featurePredictConduit =
  awaitForever (yield . second (L.map (getFeature . Dense . VU.toList)))
