{-# LANGUAGE FlexibleContexts #-}
module Application.RecenterImage.Conduit where

import           Control.Monad                as M
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           CV.Array.LabeledArray
import           CV.Filter.GaussianFilter
import           CV.Utility.Parallel
import           Data.Array.Repa              as R
import           Data.Complex
import           Data.Conduit
import           Data.Conduit.List            as CL
import           Data.Ix
import           Data.List                    as L
import           Data.Maybe
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
  :: ParallelParams
  -> GaussianFilter (R.Array U DIM2 (Complex Double))
  -> Conduit (LabeledArray DIM3 Double) (ResourceT IO) (LabeledArray DIM3 Double)
recenterFixedSizeConduit parallelParams gaussianFilter = do
  xs <- CL.take (batchSize parallelParams)
  unless
    (L.null xs)
    (do let ys =
              parMapChunk
                parallelParams
                rdeepseq
                (\(LabeledArray label x) ->
                   let (yIdx, xIdx) =
                         findCenter . applyFilterFixedSize gaussianFilter $ x
                       (Z :. nf' :. ny' :. nx') = extent x
                       arr =
                         computeUnboxedS $
                         backpermuteDft
                           (fromFunction
                              (Z :. nf' :. ny' * 2 :. nx' * 2)
                              (\_ -> 0))
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
        sourceList ys
        recenterFixedSizeConduit parallelParams gaussianFilter)


findCenterConduit
  :: (R.Source s Double)
  => ParallelParams
  -> Conduit (R.Array s DIM3 Double) (ResourceT IO) (R.Array U DIM3 Double)
findCenterConduit parallelParams = do
  xs <- CL.take (batchSize parallelParams)
  unless
    (L.null xs)
    (do let ys =
              parMapChunk
                parallelParams
                rseq
                (\x ->
                   let gaussianParams = GaussianFilterParams 32 ny' nx'
                       filteredImage = applyFilterVariedSize gaussianParams $ x
                       (yIdx, xIdx) = findCenter filteredImage
                       (Z :. nf' :. ny' :. nx') = extent x
                       arr =
                         computeUnboxedS $
                         R.traverse
                           x
                           id
                           (\f idx@(Z :. k :. j :. i) ->
                              if i == xIdx || j == yIdx
                                then 255
                                else f idx)
                   in deepSeqArray arr ((yIdx, xIdx), arr))
                xs
        liftIO . print . fst . L.unzip $ ys
        sourceList . snd . L.unzip $ ys
        findCenterConduit parallelParams)
