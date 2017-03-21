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
  generateImage (\i j -> arr R.! (Z :. j :. i))
                w'
                h'
  where h' = fromIntegral h
        w' = fromIntegral w
        arr =
          R.fromUnboxed (Z :. h' :. w')
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
                              ,applyFilter (VU.map (\x -> 0 :+ fromIntegral x) c)
                                           filterVecsList))
                        xs
                sourceList ys
                applyFilterVariedSizeConduit parallelParams
                                             polarFilterParams
                                             cartesianGratingFilterParams
                                             hyperbolicFilterParams)


{-# INLINE applyFilter #-}

applyFilter :: VU.Vector (Complex Double)
            -> [[VU.Vector (Complex Double)]]
            -> VU.Vector Double
applyFilter imgVec =
  normalizeVec .
  VU.concat .
  L.map (\filterVecs ->
           complexVec2RealVec .
           VU.fromList . L.map (VU.sum . VU.zipWith (*) imgVec) $
           filterVecs)

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
featureConduitP parallelParams =
  do xs <- CL.take (batchSize parallelParams)
     unless (L.null xs)
            (do let ys =
                      parMapChunk parallelParams
                                  rseq
                                  (second $ getFeature . Dense . VU.toList) $
                      xs
                CL.sourceList ys
                featureConduitP parallelParams)

testSink
  :: Sink OfflineCharacter (ResourceT IO) ()
testSink = awaitForever (\(OfflineCharacter _ w h _) -> liftIO . print $ (w,h))
