module Application.RotatedMNIST.Conduit where

import           Classifier.LibLinear
import           Classifier.LibSVM
import           Control.Arrow
import           Control.Monad                         as M
import           Control.Monad.IO.Class
import           Control.Monad.Parallel                as MP
import           Control.Monad.Trans.Resource
import           CV.Array.Image
import           CV.Array.LabeledArray
import           CV.Filter.FourierMellinTransform
import           CV.Filter.ShiftablePinwheelPyramid
import           CV.Filter.ShiftablePinwheelPyramidCNN
import           CV.IO.ImageIO
import           CV.Statistics.KMeans
import           CV.Statistics.PCA
import           CV.Utility.DFT                        as DFT
import           CV.Utility.Draw
import           CV.Utility.Parallel
import           CV.Utility.RepaArrayUtility           (arrayToUnboxed)
import           CV.Utility.Time
import           Data.Array.Repa                       as R
import           Data.Binary
import           Data.Complex
import           Data.Conduit
import           Data.Conduit.List                     as CL
import           Data.List                             as L
import           Data.Vector.Storable                  as VS
import           Data.Vector.Unboxed                   as VU
import           Foreign.C.String
import           Foreign.Marshal.Array
import           Foreign.Ptr

filterConduit
  :: (FilterConvolution a)
  => ParallelParams
  -> DFTPlan
  -> [a]
  -> Conduit (LabeledArray DIM3 Double) (ResourceT IO) (Double, [[VU.Vector Double]])
filterConduit parallelParams dftPlan filters = do
  xs <- CL.take (batchSize parallelParams)
  unless
    (L.null xs)
    (do let (Z :. nf :. rows :. cols) =
              (\(LabeledArray _ x) -> extent x) . L.head $ xs
        ys <-
          liftIO $
          MP.mapM
            (\(LabeledArray label x) -> do
               let imgVecs =
                     L.map (VU.convert . VU.map (:+ 0)) . arrayToUnboxed $ x
                   filterList = L.concatMap getFilterConvolutionList filters
               filteredImages <-
                 liftIO $
                 M.mapM
                   (\filter' -> applyFilterConvolution dftPlan filter' imgVecs)
                   filters
               return (fromIntegral label, filteredImages))
            xs
        let len = L.sum . L.map getFilterConvolutionNum $ filters
            zs =
              parMapChunk
                parallelParams
                rdeepseq
                (second $
                 flip (:) [] .
                 L.map VU.fromList .
                 L.transpose . L.map (VS.toList . VS.map magnitude) . L.concat) $
              ys
        sourceList zs
        filterConduit parallelParams dftPlan filters)

filterExpansionConduit
  :: (FilterExpansion a)
  => ParallelParams
  -> a
  -> Conduit (LabeledArray DIM3 Double) (ResourceT IO) (Double, VU.Vector Double)
filterExpansionConduit parallelParams filters = do
  xs <- CL.take (batchSize parallelParams)
  unless
    (L.null xs)
    (do let ys =
              parMapChunk
                parallelParams
                rdeepseq
                (\(LabeledArray label' x) ->
                   ( fromIntegral label'
                   , normalizeVec .
                     VU.map magnitude .
                     VU.fromList .
                     applyFilterExpansion filters .
                     L.map (VU.map (:+ 0)) . arrayToUnboxed $
                     x))
                xs
        sourceList ys
        filterExpansionConduit parallelParams filters)

getOriginsConduit
  :: (FilterConvolution a)
  => ParallelParams
  -> DFTPlan
  -> [a]
  -> FilePath
  -> Conduit (LabeledArray DIM3 Double) (ResourceT IO) ((Double, Double), LabeledArray DIM3 Double)
getOriginsConduit parallelParams plan filters modelPath = do
  modelName <- liftIO $ newCString modelPath
  model <- liftIO $ c'load_model modelName
  conduit model
  where
    conduit
      :: (Ptr C'model)
      -> Conduit (LabeledArray DIM3 Double) (ResourceT IO) ((Double, Double), LabeledArray DIM3 Double)
    conduit model = do
      xs <- CL.take (batchSize parallelParams)
      unless
        (L.null xs)
        (do let (Z :. nf :. rows :. cols) =
                  extent . (\(LabeledArray _ x) -> x) . L.head $ xs
                idx = [(i, j) | i <- [0 .. rows - 1], j <- [0 .. cols - 1]]
            filteredImgs <-
              liftIO $
              MP.mapM
                (\(LabeledArray _ x) -> do
                   let imgVecs =
                         L.map (VU.convert . VU.map (:+ 0)) . arrayToUnboxed $ x
                       filterList = L.concatMap getFilterConvolutionList filters
                   filteredImages <-
                     liftIO $
                     M.mapM
                       (\filter' -> applyFilterConvolution plan filter' imgVecs)
                       filters
                   return filteredImages)
                xs
            let len = L.sum . L.map getFilterConvolutionNum $ filters
                ys =
                  parMapChunk
                    parallelParams
                    rdeepseq
                    (\filteredImg ->
                       let arr =
                             fromUnboxed (Z :. len * nf :. rows :. cols) .
                             VU.map magnitude .
                             VS.convert . VS.concat . L.concat $
                             filteredImg
                       in L.map
                            (\(i, j) ->
                               ( (i, j)
                               , flip VS.snoc (C'feature_node (-1) 0) .
                                 VS.imap
                                   (\k x ->
                                      C'feature_node
                                        (fromIntegral $ k + 1)
                                        (realToFrac x)) .
                                 VU.convert .
                                 normalizeVec .
                                 toUnboxed . computeS . R.slice arr $
                                 (Z :. All :. i :. j)))
                            idx)
                    filteredImgs
            zs <-
              liftIO $
              MP.mapM
                (\y -> do
                   prediction <-
                     M.mapM
                       (\(idx, vec) -> do
                          prediction <- unsafeWith vec $ c'predict model
                          return (idx, round prediction == 1))
                       y
                   return .
                     join
                       (***)
                       (\as ->
                          (fromIntegral . L.sum $ as) /
                          (fromIntegral . L.length $ as)) .
                     L.unzip . removeOutlier . fst . L.unzip . L.filter snd $
                     prediction)
                ys
            sourceList $ L.zip zs xs
            conduit model)

centerConduit
  :: Conduit (LabeledArray DIM3 Double) (ResourceT IO) ((Double, Double), LabeledArray DIM3 Double)
centerConduit =
  awaitForever
    (\x@(LabeledArray _ arr) ->
       let (Z :. _ :. rows :. cols) = extent arr
       in yield ((fromIntegral $ div rows 2, fromIntegral $ div cols 2), x))


logpolarImageConduit
  :: ParallelParams
  -> Int
  -> Int
  -> Double
  -> Bool
  -> Int
  -> Conduit ((Double, Double), LabeledArray DIM3 Double) (ResourceT IO) (Double, ShiftablePinwheelPyramidInputArray)
logpolarImageConduit parallelParams ts rs polarR logpolarFlag numGird = do
  xs <- CL.take (batchSize parallelParams)
  unless
    (L.null xs)
    (do let ys =
              parMapChunk
                parallelParams
                rdeepseq
                (\((i, j), LabeledArray label' x) ->
                   let (Z :. _ :. rows :. cols) = extent x
                       centers =
                         (,) <$> (generateCenters (round i) rows numGird) <*>
                         (generateCenters (round j) cols numGird)
                       imgs =
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
                         computeS .
                         R.backpermute
                           (Z :. nf :. L.length centers :. ts :. rs)
                           (\(Z :. a :. b :. c :. d) -> (Z :. b :. a :. c :. d)) .
                         fromUnboxed (Z :. L.length centers :. nf :. ts :. rs) .
                         VU.concat $
                         imgs
                   in (fromIntegral label', result))
                xs
        sourceList ys
        logpolarImageConduit parallelParams ts rs polarR logpolarFlag numGird)


imageConduit
  :: ParallelParams
  -> Conduit (LabeledArray DIM3 Double) (ResourceT IO) (Double, ShiftablePinwheelPyramidInputArray)
imageConduit parallelParams = do
  xs <- CL.take (batchSize parallelParams)
  unless
    (L.null xs)
    (do let ys =
              parMapChunk
                parallelParams
                rdeepseq
                (\(LabeledArray label' x) ->
                   let (Z :. nf :. rows :. cols) = extent x
                   in ( fromIntegral label'
                      , computeS .
                        reshape (Z :. nf :. (1 :: Int) :. rows :. cols) $
                        x))
                xs
        sourceList ys
        imageConduit parallelParams)

shiftablePinwheelBlobPyramidConduit
  :: ParallelParams
  -> DFTPlan
  -> Int
  -> ShiftablePinwheelBlobPyramidFilters
  -> Conduit (Double, ShiftablePinwheelPyramidInputArray) (ResourceT IO) (Double, [[VU.Vector Double]])
shiftablePinwheelBlobPyramidConduit parallelParams plan stride' filter' = do
  xs <- CL.take (batchSize parallelParams)
  unless
    (L.null xs)
    (do ys <-
          liftIO $
          MP.mapM
            (\(label, arr) -> do
               zs <-
                 shiftablePinwheelBlobPyramid
                   plan
                   stride'
                   filter'
                   featureExtractionBlob
                   arr
               return (label, zs))
            xs
        sourceList ys
        shiftablePinwheelBlobPyramidConduit parallelParams plan stride' filter')

shiftablePinwheelBlobPyramidConduitDFT
  :: ParallelParams
  -> DFTPlan
  -> Int
  -> ShiftablePinwheelBlobPyramidFilters
  -> Conduit (Double, ShiftablePinwheelPyramidInputArray) (ResourceT IO) (Double, [[VU.Vector Double]])
shiftablePinwheelBlobPyramidConduitDFT parallelParams plan stride' filter' = do
  xs <- CL.take (batchSize parallelParams)
  unless
    (L.null xs)
    (do ys <-
          liftIO $
          MP.mapM
            (\(label, arr) -> do
               zs <-
                 shiftablePinwheelBlobPyramidDFT
                   plan
                   stride'
                   filter'
                   featureExtractionBlobDFT
                   arr
               return (label, zs))
            xs
        sourceList ys
        shiftablePinwheelBlobPyramidConduitDFT parallelParams plan stride' filter')

shiftablePinwheelBlobPyramidScatteringNetworksConduit
  :: ParallelParams
  -> DFTPlan
  -> Int
  -> Int
  -> Int
  -> Int
  -> ShiftablePinwheelBlobPyramidScatteringNetworksFilters
  -> Maybe Int
  -> Conduit (Double, ShiftablePinwheelPyramidInputArray) (ResourceT IO) (Double, [[VU.Vector Double]])
shiftablePinwheelBlobPyramidScatteringNetworksConduit parallelParams plan m stride' numLayers k filter' flag = do
  xs <- CL.take (batchSize parallelParams)
  unless
    (L.null xs)
    (do ys <-
          liftIO $
          MP.mapM
            (\(label, arr) -> do
               let (Z :. nc :. nf :. ts :. rs) = extent arr
               zs <-
                 shiftablePinwheelBlobPyramidScatteringNetworks
                   plan
                   filter'
                   m
                   stride'
                   featureExtractionBlobScattering
                   numLayers
                   k
                   [arr]
               return
                 ( label
                 , case flag of
                     Nothing ->
                       [ L.map (VU.concat . L.map rescaleVector) . L.transpose $
                         zs
                       ]
                     Just n -> [L.head . L.drop n $ zs]))
            xs
        sourceList ys
        shiftablePinwheelBlobPyramidScatteringNetworksConduit
          parallelParams
          plan
          m
          stride'
          numLayers
          k
          filter'
          flag)

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


pcaConduitSingleModel
  :: ParallelParams
  -> PCAMatrix Double
  -> Conduit (Double, VU.Vector Double) (ResourceT IO) (Double, VU.Vector Double)
pcaConduitSingleModel parallelParams pcaMat = do
  xs <- CL.take (batchSize parallelParams)
  unless
    (L.null xs)
    (do let ys =
              parMapChunk
                parallelParams
                rdeepseq
                (second $ pcaReduction pcaMat)
                xs
        sourceList ys
        pcaConduitSingleModel parallelParams pcaMat)

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
                (second $ rescaleVector . VU.concat . L.zipWith (vlad . center) models)
                xs
        sourceList ys
        kmeansConduit parallelParams models)


concatConduit
  :: ParallelParams
  -> Conduit (Double, [[VU.Vector Double]]) (ResourceT IO) (Double, [[VU.Vector Double]])
concatConduit parallelParams = do
  xs <- CL.take (batchSize parallelParams)
  unless
    (L.null xs)
    (do let ys =
              parMapChunk
                parallelParams
                rdeepseq
                (\(a, b) -> (a, [L.map VU.concat . L.transpose $ b]))
                xs
        sourceList ys
        concatConduit parallelParams)

{-# INLINE normalizeVec #-}

normalizeVec :: VU.Vector Double -> VU.Vector Double
normalizeVec vec
  | s == 0 = VU.replicate (VU.length vec) 0
  | otherwise = VU.map (/ s) vec
  where
    s = sqrt . VU.sum . VU.map (^ (2 :: Int)) $ vec

{-# INLINE removeOutlier #-}

removeOutlier :: [(Int,Int)] -> [(Int,Int)]
removeOutlier xs =
  let center =
        join (***) (\x -> fromIntegral x / fromIntegral (L.length xs)) .
        L.foldl1' (\(a, b) (c, d) -> (a + c, b + d)) $
        xs
      distFunc (a, b) (c, d) = sqrt $ (a - c) ** 2 + (b - d) ** 2
      std =
        sqrt $
        (L.sum .
         L.map (\(a, b) -> distFunc center (fromIntegral a, fromIntegral b)) $
         xs) /
        (fromIntegral (L.length xs - 1))
  in L.filter
       (\(a, b) -> distFunc center (fromIntegral a, fromIntegral b) <= (3 * std))
       xs

{-# INLINE generateCenters #-}

generateCenters :: Int -> Int -> Int -> [Double]
generateCenters center len numGrid
  | len == numGrid = L.map fromIntegral [0 .. len - 1]
  | odd numGrid = L.map (\x -> fromIntegral $ x * dist + center) [-m .. m]
  | otherwise = error "generateCenters: numGrid is even."
  -- | otherwise = L.map fromIntegral [stride,2 * stride .. n * stride]
  where
    stride = div len (numGrid + 1)
    -- center = div len 2
    dist = 4
    m = div numGrid 2

featurePtrConduit :: Conduit (a, VU.Vector Double) (ResourceT IO) (a, Ptr C'feature_node)
featurePtrConduit =
  awaitForever
    (\(label, vec) -> do
       featurePtr <- liftIO $ newArray . getFeature . Dense . VU.toList $ vec
       yield (label, featurePtr))

featureConduit :: Conduit (a, VU.Vector Double) (ResourceT IO) (a, [C'feature_node])
featureConduit = awaitForever (yield . second (getFeature . Dense . VU.toList))

remove9Conduit :: Conduit (LabeledArray DIM3 Double) (ResourceT IO) (LabeledArray DIM3 Double)
remove9Conduit =
  awaitForever (\x@(LabeledArray label _) -> unless (label == 9) (-- do liftIO $ print label
                                                                     yield x))

kmeansSink
  :: ParallelParams
  -> Int
  -> Int
  -> FilePath
  -> Double
  -> Sink (Double, [[VU.Vector Double]]) (ResourceT IO) KMeansModel
kmeansSink parallelParams numExample numGaussian kmeansFile threshold = do
  xs <- CL.take numExample
  let ys = L.concatMap L.concat . snd . L.unzip $ xs
  liftIO . kmeans parallelParams numGaussian kmeansFile threshold $ ys

svmFeatureConduit
  :: ParallelParams
  -> Conduit (Double, VU.Vector Double) (ResourceT IO) (Double, Ptr C'svm_node)
svmFeatureConduit parallelParams = do
  xs <- CL.take (batchSize parallelParams)
  unless
    (L.null xs)
    (do ys <-
          liftIO $
          M.mapM
            (\(label, vec) -> do
               let pairs = VU.snoc (VU.imap (\i v -> (i + 1, v)) vec) (-1, 0)
               y <-
                 newArray .
                 L.map (\(i, v) -> C'svm_node (fromIntegral i) (realToFrac v)) .
                 VU.toList $
                 pairs
               return (label, y))
            xs
        sourceList ys
        svmFeatureConduit parallelParams)

{-# INLINE rescaleVector #-}

rescaleVector :: VU.Vector Double -> VU.Vector Double
rescaleVector vec = VU.map (\x -> ((x - minV) / v - 0.5) * 2) vec
  where
    maxV = VU.maximum vec
    minV = VU.minimum vec
    v = maxV - minV
    


