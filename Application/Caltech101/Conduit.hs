{-# LANGUAGE FlexibleContexts #-}
module Application.Caltech101.Conduit where

import           Classifier.LibLinear
import           Control.Monad                    as M
import           Control.Monad                    as M
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           CV.Filter.CartesianGratingFilter as CF
import           CV.Filter.HyperbolicFilter       as HF
import           CV.Filter.PolarSeparableFilter   as PF
import           CV.Utility.Parallel
import           CV.Utility.RepaArrayUtility      as Utility
import           Data.Array.Repa                  as R
import           Data.Array.Repa                  as R
import           Data.Complex                     as C
import           Data.Conduit
import           Data.Conduit.List                as CL
import           Data.List                        as L
import           Data.Vector.Unboxed              as VU
import           Foreign.Marshal.Array
import           Foreign.Ptr

applyFilterCenterVariedSizeConduit
  :: (R.Source s Double)
  => ParallelParams
  -> PolarSeparableFilterParamsSet
  -> CartesianGratingFilterParams
  -> HyperbolicFilterParams
  -> Conduit (R.Array s DIM3 Double) (ResourceT IO) (VU.Vector Double)
applyFilterCenterVariedSizeConduit parallelParams params@(PolarSeparableFilterParamsSet _ downSampleFactor scaleSet rfSet afSet name) cartesianGratingFilterParams hyperbolicFilterParams = do
  xs <- CL.take (batchSize parallelParams)
  unless
    (L.null xs)
    (do let ys =
              parMapChunk
                parallelParams
                rdeepseq
                (\x ->
                    let (Z :. nf' :. ny' :. nx') = extent x
                        newNx = div nx' downSampleFactor
                        newNy = div ny' downSampleFactor
                        paramsSet =
                          PolarSeparableFilterParamsSet
                            (newNy, newNx)
                            downSampleFactor
                            scaleSet
                            rfSet
                            afSet
                            name
                        (PolarSeparableFilter _ polarSeparableFilter) =
                          makeCenterFilterSet paramsSet
                        (CartesianGratingFilter _ cartesianGratingFilter) =
                          CF.makeFilter . CF.changeSizeParams cartesianGratingFilterParams $
                          (newNx, newNy)
                        (HyperbolicFilter _ hyperbolicFilter) =
                          HF.makeFilter . HF.changeSizeParams hyperbolicFilterParams $
                          (newNx, newNy)
                        filterVecs =
                          polarSeparableFilter L.++ cartesianGratingFilter L.++
                          hyperbolicFilter
                        img =
                          downsample [downSampleFactor, downSampleFactor, 1] x
                        imgVecs =
                          L.map
                            (\i ->
                                VU.map (:+ 0) . toUnboxed . computeS . R.slice img $
                                (Z :. i :. All :. All))
                            [0 .. nf' - 1]
                    in normalizeVec . complexVec2RealVec .
                           VU.fromList .
                           L.concatMap
                             (\imgVec -> L.map (VU.sum . VU.zipWith (*) imgVec) filterVecs) $
                           imgVecs)
                xs
        sourceList ys
        applyFilterCenterVariedSizeConduit
          parallelParams
          params
          cartesianGratingFilterParams
          hyperbolicFilterParams)


featurePtrConduit :: Conduit (VU.Vector Double) (ResourceT IO) (Ptr C'feature_node)
featurePtrConduit =
  awaitForever
    (\vec -> do
       featurePtr <- liftIO $ newArray . getFeature . Dense . VU.toList $ vec
       yield featurePtr)
       
featureConduit :: Conduit (VU.Vector Double) (ResourceT IO) [C'feature_node]
featureConduit = awaitForever (yield . getFeature . Dense . VU.toList)

featureConduitP
  :: ParallelParams
  -> Conduit (VU.Vector Double) (ResourceT IO) [C'feature_node]
featureConduitP parallelParams =
  do xs <- CL.take (batchSize parallelParams)
     unless (L.null xs)
            (do let ys =
                      parMapChunk parallelParams
                                  rseq
                                  (getFeature . Dense . VU.toList)
                                  xs
                sourceList ys
                featureConduitP parallelParams)

{-# INLINE complexVec2RealVec #-}

complexVec2RealVec :: VU.Vector (Complex Double) -> VU.Vector Double
complexVec2RealVec vec = a VU.++ b
  where
    (a, b) = VU.unzip . VU.map polar $ vec

{-# INLINE normalizeVec #-}

normalizeVec :: VU.Vector Double -> VU.Vector Double
normalizeVec vec = VU.map (/s) vec
  where s = sqrt . VU.sum . VU.map (^2) $ vec
