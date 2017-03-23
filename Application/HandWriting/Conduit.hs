{-# LANGUAGE FlexibleContexts #-}
module Application.HandWriting.Conduit where

import           Application.HandWriting.IO
import           Classifier.LibLinear
import           Codec.Picture
import           Control.Arrow
import           Control.Monad                as M
import           Control.Monad.IO.Class
import           Control.Monad.Parallel       as MP
import           Control.Monad.Trans.Resource
import           CV.Utility.Parallel
import           CV.Utility.RepaArrayUtility
import           CV.V4Filter                  hiding
                                               (applyFilterVariedSizeConduit)
import           Data.Array.Repa              as R
import           Data.Binary
import           Data.ByteString              as BS
import           Data.ByteString.Lazy         as BL
import           Data.Complex
import           Data.Conduit
import           Data.Conduit.List            as CL
import           Data.List                    as L
import           Data.Vector.Unboxed          as VU
import           Foreign.Marshal.Array
import           Foreign.Ptr

plotCharacter
  :: FilePath -> OfflineCharacter -> IO ()
plotCharacter filePath (OfflineCharacter _ w h c) =
  writePng filePath $
  generateImage (\i j -> arr' R.! (Z :. j :. i))
                w'
                h'
  where h' = fromIntegral h
        w' = fromIntegral w
        arr' =
          R.fromUnboxed (Z :. h' :. w')
                        c
                        
plotSparseCharacter
  :: FilePath -> SparseOfflineCharacter -> IO ()
plotSparseCharacter filePath (SparseOfflineCharacter _ w h c) =
  writePng filePath $ generateImage (\i j -> arr' R.! (Z :. j :. i)) w' h'
  where
    h' = fromIntegral h
    w' = fromIntegral w
    arr' =
      R.fromUnboxed (Z :. h' :. w') .
      VU.accumulate (+) (VU.replicate (fromIntegral $ w * h) 0) . VU.map (second $ const (255::Word8)) .
      VU.map (first fromIntegral) $
      c

applyFilterVariedSizeConduit
  :: ParallelParams
  -> PolarSeparableFilterParamsSet
  -> CartesianGratingFilterParams
  -> HyperbolicFilterParams
  -> Conduit OfflineCharacter (ResourceT IO) (Double,VU.Vector Double)
applyFilterVariedSizeConduit parallelParams polarFilterParams cartesianGratingFilterParams hyperbolicFilterParams =
  do xs <- CL.take (batchSize parallelParams)
     unless (L.null xs)
            (do let ys =
                      parMapChunk
                        parallelParams
                        rdeepseq
                        (\(OfflineCharacter t w h c) ->
                           let rows = fromIntegral h
                               cols = fromIntegral w
                               psf =
                                 makeFilter . changeSizeParameter rows cols $
                                 PolarSeparableFilter polarFilterParams [] :: PolarSeparableFilterExpansion
                               cgf =
                                 makeFilter . changeSizeParameter rows cols $
                                 CartesianGratingFilter cartesianGratingFilterParams [] :: CartesianGratingFilter
                               hf =
                                 makeFilter . changeSizeParameter rows cols $
                                 HyperbolicFilter hyperbolicFilterParams [] :: HyperbolicFilter
                               psfVecs = getFilterVectors psf
                               cgfVecs = getFilterVectors cgf
                               hfVecs = getFilterVectors hf
                               filterVecsList = [psfVecs,cgfVecs,hfVecs]
                           in (fromIntegral t
                              ,applyFilter (VU.map (\x -> fromIntegral x :+ 0) c)
                                           filterVecsList))
                        xs
                sourceList ys
                applyFilterVariedSizeConduit parallelParams
                                             polarFilterParams
                                             cartesianGratingFilterParams
                                             hyperbolicFilterParams)
                                             
applyFilterVariedSizeGridConduit
  :: ParallelParams
  -> (Int,Int)
  -> PolarSeparableFilterParamsSet
  -> CartesianGratingFilterParams
  -> HyperbolicFilterParams
  -> Conduit OfflineCharacter (ResourceT IO) (Double,VU.Vector Double)
applyFilterVariedSizeGridConduit parallelParams gridSize polarFilterParams cartesianGratingFilterParams hyperbolicFilterParams =
  do xs <- CL.take (batchSize parallelParams)
     unless (L.null xs)
            (do let ys =
                      parMapChunk
                        parallelParams
                        rdeepseq
                        (\(OfflineCharacter t w h c) ->
                           let rows = fromIntegral h
                               cols = fromIntegral w
                               psf =
                                 makeFilterGrid gridSize .
                                 changeSizeParameter rows cols $
                                 PolarSeparableFilter polarFilterParams [] :: PolarSeparableFilterExpansion
                               cgf =
                                 makeFilterGrid gridSize .
                                 changeSizeParameter rows cols $
                                 CartesianGratingFilter cartesianGratingFilterParams [] :: CartesianGratingFilter
                               hf =
                                 makeFilterGrid gridSize .
                                 changeSizeParameter rows cols $
                                 HyperbolicFilter hyperbolicFilterParams [] :: HyperbolicFilter
                               psfVecs = getFilterVectors psf
                               cgfVecs = getFilterVectors cgf
                               hfVecs = getFilterVectors hf
                               filterVecsList = [psfVecs,cgfVecs,hfVecs]
                           in (fromIntegral t
                              ,applyFilter (VU.map (\x -> fromIntegral x :+ 0) c)
                                           filterVecsList))
                        xs
                sourceList ys
                applyFilterVariedSizeGridConduit parallelParams
                                                 gridSize
                                                 polarFilterParams
                                                 cartesianGratingFilterParams
                                                 hyperbolicFilterParams)

applyFilterfixedSizeSparseConduit
  :: ParallelParams
  -> [[VU.Vector (Complex Double)]]
  -> Conduit SparseOfflineCharacter (ResourceT IO) (Double, VU.Vector Double)
applyFilterfixedSizeSparseConduit parallelParams filterVecsList =
  do xs <- CL.take (batchSize parallelParams)
     unless (L.null xs)
            (do let ys =
                      parMapChunk
                        parallelParams
                        rdeepseq
                        (\(SparseOfflineCharacter t w h c) ->
                           (fromIntegral t
                           ,applyFilterSparse
                              (VU.map (\(i,v) ->
                                         (fromIntegral i,fromIntegral v :+ 0))
                                      c)
                              filterVecsList))
                        xs
                sourceList ys
                applyFilterfixedSizeSparseConduit parallelParams filterVecsList)

{-# INLINE applyFilter #-}

applyFilter :: VU.Vector (Complex Double)
            -> [[VU.Vector (Complex Double)]]
            -> VU.Vector Double
applyFilter imgVec =
  VU.concat .
  L.map (normalizeVec .
         VU.fromList .
         complexList2RealList . L.map (VU.sum . VU.zipWith (*) imgVec))

{-# INLINE applyFilterSparse #-}

applyFilterSparse :: VU.Vector (Int, Complex Double)
                  -> [[VU.Vector (Complex Double)]]
                  -> VU.Vector Double
applyFilterSparse imgVec =
  normalizeVec .
  VU.concat .
  L.map (-- normalizeVec .
         -- complexVec2RealVec .  
         VU.fromList .
         complexList2RealList .
         L.map (\filterVec ->
                  VU.sum . VU.map (\(i,v) -> (filterVec VU.! i) * v) $ imgVec)) 

{-# INLINE normalizeVec #-}

normalizeVec :: VU.Vector Double -> VU.Vector Double
normalizeVec vec = VU.map (/ s) vec
  where
    s = sqrt . VU.sum . VU.map (^ (2 :: Int)) $ vec

{-# INLINE complexVec2RealVec #-}

complexVec2RealVec :: VU.Vector (Complex Double) -> VU.Vector Double
complexVec2RealVec vec = a VU.++ b
  where
    (a, b) = VU.unzip . VU.map polar $ vec
    

{-# INLINE complexList2RealList #-}

complexList2RealList :: [Complex Double] -> [Double]
complexList2RealList xs = a L.++ b
  where
    (a, b) = L.unzip . L.map polar $ xs


featurePtrConduitP
  :: ParallelParams
  -> Conduit (Double,VU.Vector Double) (ResourceT IO) (Double,Ptr C'feature_node)
featurePtrConduitP parallelParams =
  do xs <- CL.take (batchSize parallelParams)
     unless (L.null xs)
            (do let (as,bs) = L.unzip xs
                ys <-
                  liftIO . MP.mapM (newArray . getFeature . Dense . VU.toList) $ bs
                CL.sourceList $ L.zip as ys
                featurePtrConduitP parallelParams)

featureConduitP
  :: ParallelParams
  -> Conduit (Double, VU.Vector Double) (ResourceT IO) (Double, [C'feature_node])
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

testSink
  :: Sink OfflineCharacter (ResourceT IO) ()
testSink = awaitForever (\(OfflineCharacter _ w h _) -> liftIO . print $ (w,h))

extractRangeConduit :: (Int, Int) -> Conduit OfflineCharacter (ResourceT IO) OfflineCharacter
extractRangeConduit (labelMin, labelMax) =
  awaitForever
    (\c@(OfflineCharacter t _ _ _) ->
        when
          (fromIntegral t >= labelMin && fromIntegral t <= labelMax)
          (yield c))
          
extractRangeSparseConduit
  :: (Int,Int)
  -> Conduit SparseOfflineCharacter (ResourceT IO) SparseOfflineCharacter
extractRangeSparseConduit (labelMin,labelMax) =
  awaitForever
    (\c@(SparseOfflineCharacter t _ _ _) ->
       when (fromIntegral t >= labelMin && fromIntegral t <= labelMax)
            (yield c))

rescaleConduit
  :: ParallelParams
  -> Int
  -> Conduit OfflineCharacter (ResourceT IO) BS.ByteString
rescaleConduit parallelParams newSize = do
  xs <- CL.take (batchSize parallelParams)
  unless
    (L.null xs)
    (do let ys =
              parMapChunk
                parallelParams
                rdeepseq
                (\(OfflineCharacter t w h c) ->
                    let arr' =
                          fromUnboxed
                            (Z :. (fromIntegral h :: Int) :.
                             (fromIntegral w :: Int)) .
                          VU.map fromIntegral $
                          c
                        maxSize = fromIntegral $ max h w
                        paddedArr = pad [maxSize, maxSize] arr'
                        rescaledArr = rescale2D (newSize, newSize) (0, 255) paddedArr
                        sparseVec =
                          VU.filter (\x' -> snd x' /= 0) .
                          VU.zip
                            (VU.generate (newSize ^ (2 :: Int)) fromIntegral) .
                          toUnboxed . computeS . R.map round $
                          rescaledArr
                        sparseCharacter =
                          SparseOfflineCharacter
                            t
                            (fromIntegral newSize)
                            (fromIntegral newSize)
                            sparseVec
                        bsData = encode sparseCharacter
                        len' = fromIntegral . BL.length $ bsData :: Word32
                        bsLen = encode len'
                    in toStrict (BL.append bsLen bsData))
                xs
        sourceList ys
        rescaleConduit parallelParams newSize)
