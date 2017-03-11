{-# LANGUAGE FlexibleContexts #-}
module Application.FacialExpression.Conduit where

import           Classifier.LibLinear
import           Codec.Picture
import           Control.Monad                  as M
import           Control.Monad.IO.Class
import           Control.Monad.Parallel         as MP
import           Control.Monad.Trans.Resource
import           CV.Filter.PolarSeparableFilter
import           CV.Utility.Parallel
import           CV.Utility.RepaArrayUtility    as Utility
import           CV.Utility.Time
import           Data.Array.Repa                as R
import           Data.Complex                   as C
import           Data.Conduit                   as C
import           Data.Conduit.List              as CL
import           Data.List                      as L
import           Data.Vector.Unboxed            as VU
import           Foreign.Marshal.Array
import           Foreign.Ptr
import           System.Random

data CKImage =
  CKImage {imagePath     :: !String
          ,imageLabel    :: !Int
          ,imageLandmark :: !((Int,Int),(Int,Int)) -- ((x,y),(len_x,len_y)
          }
  deriving (Show,Read)


labelSource' :: FilePath -> C.Source (ResourceT IO) Int
labelSource' filePath = do
  xs <- liftIO . readFile $ filePath
  let ys = L.map (\x -> read x :: CKImage) . L.lines $ xs
  sourceList . L.map imageLabel  $ ys

landmarksSource
  :: FilePath -> C.Source (ResourceT IO) ((Int,Int),(Int,Int))
landmarksSource filePath =
  do xs <- liftIO . readFile $ filePath
     let ys = L.map (\x -> read x :: CKImage) . L.lines $ xs
     sourceList . L.map imageLandmark $ ys


filePathSource :: FilePath -> C.Source (ResourceT IO) FilePath
filePathSource filePath =
  do xs <- liftIO . readFile $ filePath
     let ys = L.map (\x -> read x :: CKImage) . L.lines $ xs
     sourceList . L.map imagePath $ ys

cropConduit
  :: (R.Source s Double)
  => ParallelParams
  -> Conduit (((Int,Int),(Int,Int)),R.Array s DIM3 Double) (ResourceT IO) (R.Array U DIM3 Double)
cropConduit parallelParams =
  do xs <- CL.take (batchSize parallelParams)
     unless (L.null xs)
            (do let ys =
                      parMapChunk
                        parallelParams
                        rseq
                        (\(((xMin,yMin),(xLen,yLen)),arr) ->
                           let (Z :. nf' :. _ :. _) = extent arr
                               croppedArr =
                                 computeS $
                                 Utility.crop [xMin,yMin,(0 :: Int)]
                                              [xLen,yLen,nf']
                                              arr
                           in deepSeqArray croppedArr croppedArr)
                        xs
                sourceList ys
                cropConduit parallelParams)


cropRescaleConduit
  :: (R.Source s Double)
  => ParallelParams
  -> Conduit (((Int,Int),(Int,Int)),R.Array s DIM3 Double) (ResourceT IO) (R.Array U DIM3 Double)
cropRescaleConduit parallelParams =
  do xs <- CL.take (batchSize parallelParams)
     unless (L.null xs)
            (do ys <-
                  liftIO $
                  M.mapM (\(((xMin,yMin),(xLen,yLen)),arr) ->
                             do a <- randomRIO (0.25,2)
                                b <- randomRIO (0,128)
                                let (Z :. nf' :. _ :. _) = extent arr
                                    croppedArr =
                                      computeS .
                                      R.map (\x -> a * x + b) .
                                      Utility.crop [xMin,yMin,(0 :: Int)]
                                                   [xLen,yLen,nf'] $
                                      arr
                                return $! deepSeqArray croppedArr croppedArr)
                          xs
                sourceList ys
                cropRescaleConduit parallelParams)


cropSquareConduit
  :: (R.Source s Double)
  => ParallelParams
  -> Int
  -> Conduit (((Int, Int), (Int, Int)), R.Array s DIM3 Double) (ResourceT IO) (R.Array U DIM3 Double)
cropSquareConduit parallelParams size' = do
  xs <- CL.take (batchSize parallelParams)
  unless
    (L.null xs)
    (do let ys =
              parMapChunk
                parallelParams
                rseq
                (\(((xMin, yMin), (xLen, yLen)), arr) ->
                    let (Z :. nf' :. _ :. _) = extent arr
                        newXMin =
                          if xLen >= yLen
                            then xMin
                            else xMin - div (yLen - xLen) 2
                        newYMin =
                          if yLen >= xLen
                            then yMin
                            else yMin - div (xLen - yLen) 2
                        len = max xLen yLen
                        croppedArr =
                          Utility.crop
                            [newXMin, newYMin, 0 :: Int]
                            [len, len, nf']
                            arr
                        rescaledArr = rescale25D (size', size') (0, 255) croppedArr
                    in deepSeqArray rescaledArr rescaledArr)
                xs
        sourceList ys
        cropSquareConduit parallelParams size')

pixelConduit
  :: (R.Source s Double)
  => ParallelParams
  -> Int
  -> Conduit (R.Array s DIM3 Double) (ResourceT IO) (VU.Vector Double)
pixelConduit parallelParams downSampleFactor =
  do xs <- CL.take (batchSize parallelParams)
     unless (L.null xs)
            (do let ys =
                      parMapChunk
                        parallelParams
                        rdeepseq
                        (\x ->
                           let img =
                                 downsample
                                   [downSampleFactor,downSampleFactor,1]
                                   x
                           in normalizeVec . toUnboxed . computeS $ img)
                        xs
                sourceList ys
                pixelConduit parallelParams downSampleFactor)


applyFilterCenterVariedSizeConduit
  :: (R.Source s Double)
  => ParallelParams
  -> PolarSeparableFilterParamsSet
  -> Conduit (R.Array s DIM3 Double) (ResourceT IO) (VU.Vector (Double))
applyFilterCenterVariedSizeConduit parallelParams params@(PolarSeparableFilterParamsSet _ downSampleFactor scaleSet rfSet afSet name) =
  do xs <- CL.take (batchSize parallelParams)
     unless (L.null xs)
            (do let ys =
                      parMapChunk
                        parallelParams
                        rdeepseq
                        (\x ->
                           let (Z :. nf' :. ny' :. nx') = extent x
                               (Z :. nfFilter :. _ :. _) = extent filterArr
                               newNx = div nx' downSampleFactor
                               newNy = div ny' downSampleFactor
                               paramsSet =
                                 PolarSeparableFilterParamsSet (newNy,newNx)
                                                               downSampleFactor
                                                               scaleSet
                                                               rfSet
                                                               afSet
                                                               name
                               (PolarSeparableFilter _ filterArr) =
                                 makeCenterFilterSet paramsSet
                               img =
                                 downsample
                                   [downSampleFactor,downSampleFactor,1]
                                   x
                           in normalizeVec . complexVec2RealVec . VU.fromList $
                              [VU.sum $
                               VU.zipWith
                                 (*)
                                 (VU.map (:+ 0) .
                                  toUnboxed . computeS . R.slice img $
                                  (Z :. j :. All :. All))
                                 (toUnboxed . computeS . R.slice filterArr $
                                  (Z :. i :. All :. All))
                              |j <- [0 .. nf' - 1]
                              ,i <- [0 .. nfFilter - 1]])
                        xs
                liftIO printCurrentTime
                sourceList ys
                applyFilterCenterVariedSizeConduit parallelParams params)


-- applyFilterCenterVariedSizeConduit
--   :: (R.Source s Double)
--   => ParallelParams
--   -> PolarSeparableFilterParamsSet
--   -> Conduit (R.Array s DIM3 Double) (ResourceT IO) (VU.Vector (Complex Double))
-- applyFilterCenterVariedSizeConduit parallelParams params@(PolarSeparableFilterParamsSet _ downSampleFactor scaleSet rfSet afSet name) =
--   awaitForever
--     (\x ->
--        do let (Z :. nf' :. ny' :. nx') = extent x
--               (Z :. nfFilter :. _ :. _) = extent filterArr
--               paramsSet =
--                 PolarSeparableFilterParamsSet (div ny' downSampleFactor,div nx' downSampleFactor)
--                                               downSampleFactor
--                                               scaleSet
--                                               rfSet
--                                               afSet
--                                               name
--               (PolarSeparableFilter _ filterArr) =
--                 makeCenterFilterSet paramsSet
--           y <-
--             sumP .
--             sumS .
--             traverse2 filterArr
--                       (downsample [downSampleFactor,downSampleFactor,1] x)
--                       (\(Z :. nf1 :. _ :. _) (Z :. nf2 :. _ :. _) ->
--                          (Z :. nf1 * nf2 :. div ny' downSampleFactor :. div nx' downSampleFactor)) $
--             \fFilter fImg (Z :. k :. j :. i) ->
--               let kImg = div k nfFilter
--                   kFilter = mod k nfFilter
--               in ((:+ 0) . fImg $ (Z :. kImg :. j :. i)) *
--                  fFilter (Z :. kFilter :. j :. i)
--           liftIO printCurrentTime
--           yield . toUnboxed $ y)


{-# INLINE complexVec2RealVec #-}

complexVec2RealVec :: VU.Vector (Complex Double) -> VU.Vector Double
complexVec2RealVec vec = a VU.++ b
  where
    (a, b) = VU.unzip . VU.map polar $ vec

{-# INLINE complexVec2PhaseVec #-}

complexVec2PhaseVec :: VU.Vector (Complex Double) -> VU.Vector Double
complexVec2PhaseVec = VU.map phase

{-# INLINE makeCenterFilterSet #-}

makeCenterFilterSet
  :: PolarSeparableFilterParamsSet
  -> PolarSeparableFilter PolarSeparableFilterParamsSet (R.Array U DIM3 (C.Complex Double))
makeCenterFilterSet params@(PolarSeparableFilterParamsSet (ny, nx) _downSampleFactor scaleSet rfSet afSet _name) =
  PolarSeparableFilter params $
  R.fromListUnboxed (Z :. getFilterNum params :. ny :. nx) filterEleList
  where
    paramsList = generateParamsSet scaleSet rfSet afSet
    filterEleList =
      L.concatMap
        (\(scale, rf, af) ->
            makeCenterFilterList ny nx (getFilterSetFunc params scale rf af))
        paramsList


{-# INLINE makeCenterFilterList #-}

makeCenterFilterList :: Int -> Int -> (Int -> Int -> a) -> [a]
makeCenterFilterList ny nx f =
  [ f (r - div ny 2) (c - div nx 2)
  | r <- [0 .. ny - 1]
  , c <- [0 .. nx - 1] ]


{-# INLINE featurePtrConduit #-}

featurePtrConduit :: Conduit (VU.Vector Double) (ResourceT IO) (Ptr C'feature_node)
featurePtrConduit =
  awaitForever
    (\vec -> do
       featurePtr <- liftIO $ newArray . getFeature . Dense . VU.toList $ vec
       liftIO printCurrentTime
       yield featurePtr)


featurePtrConduitP
  :: ParallelParams
  -> Conduit (VU.Vector Double) (ResourceT IO) (Ptr C'feature_node)
featurePtrConduitP parallelParams =
  do xs <- CL.take (batchSize parallelParams)
     unless (L.null xs)
            (do ys <-
                  liftIO . MP.mapM (newArray . getFeature . Dense . VU.toList) $
                  xs
                CL.sourceList ys
                featurePtrConduitP parallelParams)

featureConduitP
  :: ParallelParams
  -> Conduit (VU.Vector Double) (ResourceT IO) [C'feature_node]
featureConduitP parallelParams =
  do xs <- CL.take (batchSize parallelParams)
     unless (L.null xs)
            (do let ys =
                      parMapChunk parallelParams
                                  rseq
                                  (getFeature . Dense . VU.toList) $
                      xs
                CL.sourceList ys
                featureConduitP parallelParams)


{-# INLINE normalizeVec #-}

normalizeVec :: VU.Vector Double -> VU.Vector Double
normalizeVec vec = VU.map (/s) vec
  where s = sqrt . VU.sum . VU.map (^2) $ vec
