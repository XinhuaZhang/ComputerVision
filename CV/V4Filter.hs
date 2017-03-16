{-# LANGUAGE FlexibleContexts #-}

module CV.V4Filter
  ( module V4
  , applyFilterVariedSizeConduit
  , applyFilterFixedSizeConduit
  ) where

import           Control.Monad                    as M
import           Control.Monad.Trans.Resource
import           CV.Filter.CartesianGratingFilter as V4 
import           CV.Filter.HyperbolicFilter       as V4 
import           CV.Filter.PolarSeparableFilter   as V4 hiding (makeFilter)
import           CV.FilterExpansion               as V4
import           CV.Utility.Parallel
import           CV.Utility.RepaArrayUtility
import           Data.Array.Repa                  as R
import           Data.Complex
import           Data.Conduit
import           Data.Conduit.List                as CL
import           Data.List                        as L
import           Data.Vector.Unboxed              as VU

applyFilterVariedSizeConduit
  :: (R.Source s Double)
  => ParallelParams
  -> PolarSeparableFilterParamsSet
  -> CartesianGratingFilterParams
  -> HyperbolicFilterParams
  -> Conduit (R.Array s DIM3 Double) (ResourceT IO) (VU.Vector Double)
applyFilterVariedSizeConduit parallelParams polarFilterParams cartesianGratingFilterParams hyperbolicFilterParams = do
  xs <- CL.take (batchSize parallelParams)
  unless
    (L.null xs)
    (do let ys =
              parMapChunk
                parallelParams
                rdeepseq
                (\x ->
                    let (Z :. channels :. rows :. cols) = extent x
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
                        filterVecsList = [psfVecs, cgfVecs, hfVecs]
                        downSampleFactor = getDownsampleFactorSet polarFilterParams
                        img =
                          downsample [downSampleFactor, downSampleFactor, 1] x
                        imgVecs =
                          L.map
                            (\i ->
                                VU.map (:+ 0) . toUnboxed . computeS . R.slice img $
                                (Z :. i :. All :. All))
                            [0 .. channels - 1]
                    in applyFilter imgVecs filterVecsList)
                xs
        sourceList ys
        applyFilterVariedSizeConduit
          parallelParams
          polarFilterParams
          cartesianGratingFilterParams
          hyperbolicFilterParams)

applyFilterFixedSizeConduit
  :: (R.Source s Double)
  => ParallelParams
  -> Int
  -> [[VU.Vector (Complex Double)]]
  -> Conduit (R.Array s DIM3 Double) (ResourceT IO) (VU.Vector Double)
applyFilterFixedSizeConduit parallelParams downSampleFactor filterVecsList = do
  xs <- CL.take (batchSize parallelParams)
  unless
    (L.null xs)
    (do let ys =
              parMapChunk
                parallelParams
                rdeepseq
                (\x ->
                    let (Z :. channels :. _ :. _) = extent x
                        img =
                          downsample [downSampleFactor, downSampleFactor, 1] x
                        imgVecs =
                          L.map
                            (\i ->
                                VU.map (:+ 0) . toUnboxed . computeS . R.slice img $
                                (Z :. i :. All :. All))
                            [0 .. channels - 1]
                    in applyFilter imgVecs filterVecsList)
                xs
        sourceList ys
        applyFilterFixedSizeConduit parallelParams downSampleFactor filterVecsList)

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

{-# INLINE applyFilter #-}

applyFilter :: [VU.Vector (Complex Double)]
            -> [[VU.Vector (Complex Double)]]
            -> VU.Vector Double
applyFilter imgVecs =
  VU.concat .
  L.map
    (\filterVecs ->
        normalizeVec .
        complexVec2RealVec .
        VU.fromList .
        L.concatMap
          (\imgVec -> L.map (VU.sum . VU.zipWith (*) imgVec) filterVecs) $
        imgVecs)
