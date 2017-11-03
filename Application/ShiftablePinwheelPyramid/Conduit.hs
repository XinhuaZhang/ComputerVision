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
import           CV.IO.ImageIO
import           CV.Statistics.KMeans
import           CV.Statistics.PCA
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
                                        polarR
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

logpolarImageConduit1
  :: ParallelParams
  -> Int
  -> Int
  -> [(Double, Double)]
  -> Double
  -> Bool
  -> Conduit ImageRepa (ResourceT IO) ShiftablePinwheelPyramidInputArray
logpolarImageConduit1 parallelParams ts rs centers polarR logpolarFlag = do
  xs <- CL.take (batchSize parallelParams)
  unless
    (L.null xs)
    (do let ys =
              parMapChunk
                parallelParams
                rseq
                (\(Image _ x) ->
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
                                        polarR
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
                   in deepSeqArray result result)
                xs
        sourceList ys
        logpolarImageConduit1 parallelParams ts rs centers polarR logpolarFlag)

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
  :: FFTW
  -> Int
  -> ShiftablePinwheelBlobPyramidFilters
  -> Conduit (Double, ShiftablePinwheelPyramidInputArray) (ResourceT IO) (Double, [[VU.Vector Double]])
shiftablePinwheelBlobPyramidConduit fftw stride' filter' =
  awaitForever
    (\(label', x) -> do
       features <-
         liftIO $
         shiftablePinwheelBlobPyramid
           fftw
           stride'
           filter'
           featureExtractionBlob
           x
       yield (label', features))

shiftablePinwheelConduit
  :: FFTW
  -> Int
  -> Conduit (Double, ShiftablePinwheelPyramidInputArray) (ResourceT IO) (Double, [VU.Vector Double])
shiftablePinwheelConduit fftw downsampleFactor =
  awaitForever
    (\(label', x) -> do
       y <- liftIO $ shiftablePinwheel fftw downsampleFactor x
       yield (label', y))


shiftablePinwheelRingLogGaborConduit
  :: FFTW
  -> ShiftablePinwheelRingLogGaborFilters
  -> Conduit (Double, ShiftablePinwheelPyramidInputArray) (ResourceT IO) (Double, [[VU.Vector Double]])
shiftablePinwheelRingLogGaborConduit fftw filter' =
  awaitForever
    (\(label', x) -> do
       features <-
         liftIO $
         shiftablePinwheelRingLogGabor fftw filter' featureExtractionRing x
       yield (label', features))


pcaConduit
  :: ParallelParams
  -> [PCAMatrix Double]
  -> Conduit (Double, [[VU.Vector Double]]) (ResourceT IO) (Double, [[VU.Vector Double]])
pcaConduit parallelParams pcaMat = do
  xs <- CL.take (batchSize parallelParams)
  unless
    (L.null xs)
    (do let ys =
              parMapChunk
                parallelParams
                rdeepseq
                (second $
                 L.zipWith (\mat zs -> L.map (pcaReduction mat) zs) pcaMat)
                xs
        sourceList ys
        pcaConduit parallelParams pcaMat)
        
pcaConduit1
  :: ParallelParams
  -> PCAMatrix Double
  -> Conduit (Double, [VU.Vector Double]) (ResourceT IO) (Double, [VU.Vector Double])
pcaConduit1 parallelParams pcaMat = do
  xs <- CL.take (batchSize parallelParams)
  unless
    (L.null xs)
    (do let ys =
              parMapChunk
                parallelParams
                rdeepseq
                (second $ L.map (pcaReduction pcaMat))
                xs
        sourceList ys
        pcaConduit1 parallelParams pcaMat)

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
                (second $ VU.concat . L.zipWith (vlad . center) models
                 -- $
                 -- (\zs ->
                 --    let as =
                 --          VU.concat . L.zipWith (vlad . center) models . L.init $
                 --          zs
                 --        bs = l2norm . L.head . L.last $ zs
                 --    in VU.concat $ [as, bs])
                 )
                xs
        sourceList ys
        kmeansConduit parallelParams models)


kmeansConduit1
  :: ParallelParams
  -> KMeansModel
  -> Conduit (Double, [VU.Vector Double]) (ResourceT IO) (Double, VU.Vector Double)
kmeansConduit1 parallelParams model = do
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
        kmeansConduit1 parallelParams model)

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
  | odd n = L.map (\x -> fromIntegral $ x * dist + center) [-m .. m]
  | otherwise = error "generateCenters: numGrid is even."
  -- | otherwise = L.map fromIntegral [stride,2 * stride .. n * stride]
  where
    stride = div len (n + 1)
    center = div len 2
    dist = 4
    m = div n 2


{-# INLINE l2norm #-}

l2norm :: VU.Vector Double -> VU.Vector Double
l2norm vec
  | VU.null vec = error "l2norm: empty vector."
  | VU.all (== 0) vec = vec
  | otherwise = VU.map (/ norm) vec
  where
    norm = sqrt . VU.sum . VU.map (^ (2 :: Int)) $ vec



generateShiftablePinwheelBlobPyramidFilters2Layers :: ShiftablePinwheelBlobPyramidParams
                                                   -> [ShiftablePinwheelBlobPyramidFilters]
generateShiftablePinwheelBlobPyramidFilters2Layers params@(ShiftablePinwheelBlobPyramidParams nLayer nCenter nChannel nT nR k) =
  (generateShiftablePinwheelBlobPyramidFilters1 params) :
  (L.map
     (\i ->
        generateShiftablePinwheelBlobPyramidFilters1
          (ShiftablePinwheelBlobPyramidParams
             nLayer
             nCenter
             (nChannel * k)
             (div nT (2 ^ i))
             (div nR (2 ^ i))
             k))
     [0 .. nLayer - 1])


shiftablePinwheelBlobPyramidConduit2Layers
  :: FFTW
  -> Int
  -> [ShiftablePinwheelBlobPyramidFilters]
  -> Conduit (Double, ShiftablePinwheelPyramidInputArray) (ResourceT IO) (Double, [[VU.Vector Double]])
shiftablePinwheelBlobPyramidConduit2Layers fftw stride' filters =
  awaitForever
    (\(label', x) -> do
       let (Z :. nc :. nf :. ts :. rs) = extent x
       firstLayer <-
         liftIO $
         shiftablePinwheelBlobPyramidArray fftw (L.head filters) .
         computeS .
         R.backpermute
           (Z :. nf :. nc :. ts :. rs)
           (\(Z :. a :. b :. c :. d) -> (Z :. b :. a :. c :. d)) $
         x
       secondLayer <-
         liftIO $
         M.zipWithM
           (shiftablePinwheelBlobPyramidArray fftw)
           filters
           (L.init firstLayer)
       let y =
             parMap
               rdeepseq
               (featureExtractionBlobMag stride')
               ((L.map L.last secondLayer) L.++  ([L.last firstLayer]))
       yield (label', y))
