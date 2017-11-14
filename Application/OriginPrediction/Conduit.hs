module Application.OriginPrediction.Conduit where

import           Classifier.LibLinear
import           Control.Arrow
import           Control.Monad                    as M
import           Control.Monad.IO.Class
import           Control.Monad.Parallel           as MP
import           Control.Monad.Trans.Resource
import           CV.Array.LabeledArray
import           CV.Filter.FourierMellinTransform
import           CV.IO.ImageIO
import           CV.Utility.DFT
import           CV.Utility.Draw
import           CV.Utility.Parallel
import           CV.Utility.RepaArrayUtility      (arrayToUnboxed)
import           CV.Utility.Time
import           Data.Array.Repa                  as R
import           Data.Binary
import           Data.Complex
import           Data.Conduit
import           Data.Conduit.List                as CL
import           Data.List                        as L
import           Data.Vector.Storable             as VS
import           Data.Vector.Unboxed              as VU
import           Foreign.C.String
import           Foreign.Marshal.Array
import           Foreign.Ptr


filterConduit
  :: (FilterConvolution a)
  => ParallelParams
  -> DFTPlan
  -> [a]
  -> Conduit (LabeledArray DIM3 Double) (ResourceT IO) (R.Array U DIM3 Double)
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
               return filteredImages)
            xs
        let len = L.sum . L.map getFilterConvolutionNum $ filters
            zs =
              L.map (fromUnboxed (Z :. len * nf :. rows :. cols)) .
              parMapChunk
                parallelParams
                rdeepseq
                (VU.map magnitude . VS.convert . VS.concat . L.concat) $
              ys
        sourceList zs
        filterConduit parallelParams dftPlan filters)

{-# INLINE notOrigin #-}

notOrigin :: Int -> Int -> Int -> Bool
notOrigin origin len x = x < (origin - div len 2) || x > (origin + div len 2)

{-# INLINE getOrigins #-}

getOrigins :: Int -> R.Array U DIM3 Double -> ([(Int, Int)], [(Int, Int)])
getOrigins len arr =
  L.partition
    (\(i, j) -> notOrigin (div rows 2) len i || notOrigin (div cols 2) len j) $
  [(i, j) | i <- [0 .. rows - 1], j <- [0 .. cols - 1]]
  where
    (Z :. nf :. rows :. cols) = extent arr

{-# INLINE getStrideOrigins #-}

getStrideOrigins :: Int
                 -> Int
                 -> R.Array U DIM3 Double
                 -> ([(Int, Int)], [(Int, Int)])
getStrideOrigins stride len =
  first (L.filter (\(i, j) -> (mod i stride == 0) && (mod j stride == 0))) .
  getOrigins len

splitOriginsConduit
  :: ParallelParams
  -> Int
  -> Int
  -> Conduit (R.Array U DIM3 Double) (ResourceT IO) (Double, VU.Vector Double)
splitOriginsConduit parallelParams len stride = do
  xs <- CL.take (batchSize parallelParams)
  unless
    (L.null xs)
    (do let ys =
              parMapChunk
                parallelParams
                rdeepseq
                (\arr ->
                   let (nonCenter, center) =
                         join
                           (***)
                           (L.map
                              (\(i, j) ->
                                 normalizeVec .
                                 toUnboxed . computeS . R.slice arr $
                                 (Z :. All :. i :. j))) $
                         getStrideOrigins stride len arr
                   in (L.map ((,) 0) nonCenter) L.++ L.map ((,) 1) center)
                xs
        sourceList . L.concat $ ys
        splitOriginsConduit parallelParams len stride)


getOriginsConduit
  :: ParallelParams
  -> FilePath
  -> Conduit (R.Array U DIM3 Double) (ResourceT IO) [(Int, Int)]
getOriginsConduit parallelParams modelPath = do
  modelName <- liftIO $ newCString modelPath
  model <- liftIO $ c'load_model modelName
  conduit model
  where
    conduit :: (Ptr C'model)
            -> Conduit (R.Array U DIM3 Double) (ResourceT IO) [(Int, Int)]
    conduit model = do
      xs <- CL.take (batchSize parallelParams)
      unless
        (L.null xs)
        (do let (Z :. _ :. rows :. cols) = extent . L.head $ xs
                idx = [(i, j) | i <- [0 .. rows - 1], j <- [0 .. cols - 1]]
                ys =
                  parMapChunk
                    parallelParams
                    rdeepseq
                    (\arr ->
                       L.map
                         (\(i, j) ->
                            ( (i, j)
                            , flip VS.snoc (C'feature_node (-1) 0) .
                              VS.imap
                                (\k x ->
                                   C'feature_node
                                     (fromIntegral $ k + 1)
                                     (realToFrac x)) .
                              VU.convert .
                              normalizeVec . toUnboxed . computeS . R.slice arr $
                              (Z :. All :. i :. j)))
                         idx)
                    xs
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
                   return . fst . L.unzip . L.filter snd $ prediction)
                ys
            sourceList zs
            conduit model)


drawConduit
  :: ParallelParams
  -> Conduit (LabeledArray DIM3 Double, [(Int, Int)]) (ResourceT IO) ImageRepa
drawConduit parallelParams = do
  xs <- CL.take (batchSize parallelParams)
  unless
    (L.null xs)
    (do let ys =
              parMapChunk
                parallelParams
                rseq
                (\(LabeledArray _ x, idx') ->
                   let img = Image 8 x
                       idx = removeOutlier idx'
                       shape = L.map Point idx
                       (a, b) =
                         join
                           (***)
                           (\as ->
                              (fromIntegral . L.sum $ as) /
                              (fromIntegral . L.length $ as)) .
                         L.unzip $
                         idx
                   in draw (draw img Yellow shape) Red [Circle 2 1 (a, b)])
                xs
        sourceList ys
        drawConduit parallelParams)

plotSink :: FilePath -> Sink (Int,ImageRepa) (ResourceT IO) ()
plotSink prefixStr =
  awaitForever
    (\(i, img) ->
       liftIO $ plotImageRepa (prefixStr L.++ "_" L.++ show i L.++ ".png") img)

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

skipConduit :: Int -> Conduit a (ResourceT IO) a
skipConduit n = do
  CL.drop (n - 1)
  x <- await
  case x of
    Nothing -> return ()
    Just y -> do
      yield y
      skipConduit n

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
