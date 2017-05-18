{-# LANGUAGE FlexibleContexts #-}
module Application.RecenterImage.Conduit where

import           Control.Concurrent.MVar      (MVar)
import           Control.Monad                as M
import           Control.Monad.IO.Class
import           Control.Monad.Parallel       as MP
import           Control.Monad.Trans.Resource
import           CV.Array.LabeledArray
import           CV.Filter.GaussianFilter     as Gaussian
import           CV.Utility.Parallel
import           CV.V4Filter
import           Data.Array.Repa              as R
import           Data.Complex
import           Data.Conduit
import           Data.Conduit.List            as CL
import           Data.Ix
import           Data.List                    as L
import           Data.Ord
import           Data.Vector.Unboxed          as VU

{-# INLINE findCenter #-}

findCenter
  :: (R.Source s Double)
  => R.Array s DIM3 Double -> (Int, Int)
findCenter arr =
  fst .
  VU.maximumBy (comparing snd) .
  VU.zip (VU.fromList . range $ ((0, 0), (ny' - 1, nx' - 1))) .
  L.foldl1' (VU.zipWith (+)) .
  L.map (\i -> toUnboxed . computeS . R.slice arr $ (Z :. i :. All :. All)) $
  [0 .. nf' - 1]
  where
    (Z :. nf' :. ny' :. nx') = extent arr


recenterFixedSizeConduit
  :: MVar ()
  -> ParallelParams
  -> GaussianFilter (R.Array U DIM2 (Complex Double))
  -> Conduit (LabeledArray DIM3 Double) (ResourceT IO) (LabeledArray DIM3 Double)
recenterFixedSizeConduit lock parallelParams gaussianFilter = do
  xs <- CL.take (batchSize parallelParams)
  unless
    (L.null xs)
    (do centers <-
          liftIO $
          MP.mapM
            (\(LabeledArray _ x) ->
                fmap findCenter . Gaussian.applyFilterFixedSize lock gaussianFilter $
                x)
            xs
        let ys =
              parZipWithChunk
                parallelParams
                rdeepseq
                (\(LabeledArray label x) (yIdx, xIdx) ->
                    let (Z :. nf' :. ny' :. nx') = extent x
                        arr =
                          computeUnboxedS $
                          backpermuteDft
                            (fromFunction
                               (Z :. nf' :. ny' * 2 :. nx' * 2)
                               (const 0))
                            (\(Z :. k :. j :. i) ->
                                let j' = j - (ny' - yIdx)
                                    i' = i - (nx' - xIdx)
                                in if (j' < 0 || (j' >= ny')) ||
                                      (i' < 0 || (i' >= nx'))
                                     then Nothing
                                     else Just (Z :. k :. j' :. i'))
                            x
                    in LabeledArray label arr)
                xs
                centers
        sourceList ys
        recenterFixedSizeConduit lock parallelParams gaussianFilter)


findCenterConduit
  :: (R.Source s Double)
  => MVar ()
  -> ParallelParams
  -> Conduit (R.Array s DIM3 Double) (ResourceT IO) (R.Array U DIM3 Double)
findCenterConduit lock parallelParams = do
  xs <- CL.take (batchSize parallelParams)
  unless
    (L.null xs)
    (do centers <-
          liftIO $
          MP.mapM
            (\x ->
                let (Z :. _ :. ny' :. nx') = extent . L.head $ xs
                    gaussianParams = GaussianFilterParams 32 ny' nx'
                in fmap findCenter . Gaussian.applyFilterVariedSize lock gaussianParams $
                   x)
            xs
        let ys =
              parZipWithChunk
                parallelParams
                rseq
                (\x (yIdx, xIdx) ->
                    let arr =
                          computeUnboxedS $
                          R.traverse
                            x
                            id
                            (\f idx@(Z :. _ :. j :. i) ->
                                if i == xIdx || j == yIdx
                                  then 255
                                  else f idx)
                    in deepSeqArray arr ((yIdx, xIdx), arr))
                xs
                centers
        liftIO . print . fst . L.unzip $ ys
        sourceList . snd . L.unzip $ ys
        findCenterConduit lock parallelParams)


applyV4SeparableFilterLabeledArrayWithCenterConduit
  :: MVar ()
  -> ParallelParams
  -> GaussianFilter (R.Array U DIM2 (Complex Double))
  -> V4SeparableFilterParamsAxis
  -> Conduit (LabeledArray DIM3 Double) (ResourceT IO) (Double, VU.Vector Double)
applyV4SeparableFilterLabeledArrayWithCenterConduit lock parallelParams gaussianFilter filterParams = do
  xs <- CL.take (batchSize parallelParams)
  unless
    (L.null xs)
    (do centers <-
          liftIO $
          MP.mapM
            (\(LabeledArray _ x) ->
                fmap findCenter . Gaussian.applyFilterFixedSize lock gaussianFilter $
                x)
            xs
        let ys =
              parZipWithChunk
                parallelParams
                rdeepseq
                (\(LabeledArray label' x) center ->
                    let (Z :. channels :. _ :. _) = extent x
                        imgVecs =
                          L.map
                            (\i ->
                                VU.map (:+ 0) . toUnboxed . computeS . R.slice x $
                                (Z :. i :. All :. All))
                            [0 .. channels - 1]
                        filters =
                          generateV4SeparableFilterWithCenterAxis filterParams center
                        -- L.map
                        --   (generateV4SeparableFilterWithCenterAxis filterParams)
                        --   centers
                        -- (cR, cC) = center --(div numRows 2, div numCols 2)--center
                        -- centers =
                        --   [(cR + i, cC + j) | i <- [0], j <- [0]]
                        -- responses = L.map (\filters' -> VU.concat .
                        --                                 L.map
                        --                                   (\filter' ->
                        --                                      VU.concat $
                        --                                      L.map (applyV4SeparableFilter filter') imgVecs) $
                        --                                 filters') filters
                        -- (fromIntegral label', normalizeVec . L.foldl1' (VU.zipWith (+)) $ responses))
                    in ( fromIntegral label'
                       , normalizeVec .
                         VU.concat .
                         L.map
                           (\filter' ->
                               VU.concat $
                               L.map (applyV4SeparableFilter filter') imgVecs) $
                         filters))
                xs
                centers
        sourceList ys
        applyV4SeparableFilterLabeledArrayWithCenterConduit
          lock
          parallelParams
          gaussianFilter
          filterParams)



applyV4SeparableFilterComplexLabeledArrayWithCenterConduit
  :: MVar ()
  -> ParallelParams
  -> GaussianFilter (R.Array U DIM2 (Complex Double))
  -> V4SeparableFilterParamsAxis
  -> Conduit (LabeledArray DIM3 Double) (ResourceT IO) (Double, VU.Vector (Complex Double))
applyV4SeparableFilterComplexLabeledArrayWithCenterConduit lock parallelParams gaussianFilter filterParams = do
  xs <- CL.take (batchSize parallelParams)
  unless
    (L.null xs)
    (do centers <-
          liftIO $
          MP.mapM
            (\(LabeledArray _ x) ->
                fmap findCenter . Gaussian.applyFilterFixedSize lock gaussianFilter $
                x)
            xs
        let ys =
              parZipWithChunk
                parallelParams
                rdeepseq
                (\(LabeledArray label' x) center ->
                    let (Z :. channels :. _ :. _) = extent x
                        imgVecs =
                          L.map
                            (\i ->
                                VU.map (:+ 0) . toUnboxed . computeS . R.slice x $
                                (Z :. i :. All :. All))
                            [0 .. channels - 1]
                        filters =
                          generateV4SeparableFilterWithCenterAxis filterParams center
                        complexVec =
                          VU.concat .
                          L.map
                            (\filter' ->
                                VU.concat $
                                L.map (applyV4SeparableFilterComplex filter') imgVecs) $
                          filters
                        normalizedMagVec = normalizeVec . VU.map magnitude $ complexVec
                    in ( fromIntegral label'
                       , VU.zipWith
                           (\a b -> mkPolar a . phase $ b)
                           normalizedMagVec
                           complexVec))
                xs
                centers
        sourceList ys
        applyV4SeparableFilterComplexLabeledArrayWithCenterConduit
          lock
          parallelParams
          gaussianFilter
          filterParams)


{-# INLINE normalizeVec #-}

normalizeVec :: VU.Vector Double -> VU.Vector Double
normalizeVec vec
  | s == 0 = VU.replicate (VU.length vec) 0
  | otherwise = VU.map (/ s) vec
  where
    s = sqrt . VU.sum . VU.map (^ 2) $ vec
