module Application.ShiftablePinwheelPyramid.Conduit where

import           Classifier.LibLinear
import           Control.Arrow
import           Control.Monad                      as M
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           CV.Array.Image
import           CV.Array.LabeledArray
import           CV.Filter.PinwheelRing
import           CV.Filter.ShiftablePinwheelPyramid
import           CV.Image
import           CV.Statistics.KMeans
import           CV.Utility.Parallel
import           Data.Array.Repa                    as R
import           Data.Conduit
import           Data.Conduit.List                  as CL
import           Data.List                          as L
import           Data.Vector.Unboxed                as VU
import           Foreign.Marshal.Array
import           Foreign.Ptr

logpolarImageConduit
  :: ParallelParams
  -> Int
  -> Int
  -> [(Double, Double)]
  -> Double
  -> Bool
  -> Conduit (LabeledArray DIM3 Double) (ResourceT IO) (Double, ShiftablePinwheelPyramidInputArray)
logpolarImageConduit parallelParams ts rs centers polarR logpolarFlag = do
  xs <- CL.take (batchSize parallelParams)
  unless
    (L.null xs)
    (do let ys =
              parMapChunk
                parallelParams
                rseq
                (\(LabeledArray label' x) ->
                    let imgs =
                          L.map
                            (\center' ->
                                toUnboxed .
                                (if logpolarFlag
                                   then getLogpolarImage .
                                        cartesian2logpolarImage
                                          ts
                                          rs
                                          center'
                                          (log polarR)
                                   else getPolarImage .
                                        cartesian2polarImage ts rs center' polarR) .
                                CartesianImage (0, 255) $
                                x)
                            centers
                        (Z :. nf :. _ :. _) = extent x
                        result =
                          fromUnboxed (Z :. L.length centers :. nf :. ts :. rs) .
                          VU.concat $
                          imgs
                    in deepSeqArray result (fromIntegral label', result))
                xs
        sourceList ys
        logpolarImageConduit parallelParams ts rs centers polarR logpolarFlag)

shiftablePinwheelRingPyramidConduit
  :: FFTW
  -> ShiftablePinwheelRingPyramidFilters
  -> Conduit (Double, ShiftablePinwheelPyramidInputArray) (ResourceT IO) (Double, [[VU.Vector Double]])
shiftablePinwheelRingPyramidConduit fftw filter' =
  awaitForever
    (\(label', x) -> do
       features <-
         liftIO $
         shiftablePinwheelRingPyramid fftw filter' featureExtractionRing x
       yield (label', features))
       

shiftablePinwheelBlobPyramidConduit
  :: ParallelParams
  -> FFTW
  -> ShiftablePinwheelBlobPyramidFilters
  -> Conduit (Double, ShiftablePinwheelPyramidInputArray) (ResourceT IO) (Double, [[VU.Vector Double]])
shiftablePinwheelBlobPyramidConduit parallelParams fftw filter' =
  awaitForever
    (\(label', x) -> do
       features <-
         liftIO $
         shiftablePinwheelBlobPyramid
           parallelParams
           fftw
           filter'
           featureExtractionBlob
           x
       yield (label', features))


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

featurePtrConduit :: Conduit (a, VU.Vector Double) (ResourceT IO) (a, Ptr C'feature_node)
featurePtrConduit =
  awaitForever
    (\(label, vec) -> do
       featurePtr <- liftIO $ newArray . getFeature . Dense . VU.toList $ vec
       yield (label, featurePtr))

featureConduit :: Conduit (a, VU.Vector Double) (ResourceT IO) (a, [C'feature_node])
featureConduit = awaitForever (yield . second (getFeature . Dense . VU.toList))

generateCenters :: Int -> Int -> [Double]
generateCenters len n
  | len == n = L.map fromIntegral [0 .. len - 1]
  | otherwise = L.map fromIntegral [stride,2 * stride .. n * stride]
  where
    stride = div len (n + 1)
