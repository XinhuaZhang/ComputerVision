{-# LANGUAGE BangPatterns #-}
module Application.MultiDimensionalGMM.FisherKernel where

import           Application.MultiDimensionalGMM.Gaussian
import           Application.MultiDimensionalGMM.GMM
import           Application.MultiDimensionalGMM.MixtureModel
import           Control.Monad
import           Control.Monad.Trans.Resource
import           CV.Utility.Parallel
import           Data.Conduit
import           Data.Conduit.List                            as CL
import           Data.List                                    as L
import           Data.Vector.Unboxed                          as VU

fisherVectorMu :: GMM -> [VU.Vector Double] -> VU.Vector Double
fisherVectorMu gmm xs =
  VU.map (/ (sqrt . fromIntegral . L.length $ xs)) . L.foldl1' (VU.zipWith (+)) $
  L.zipWith
    (\assignment x ->
        VU.concat .
        L.zipWith
          (\a (Model (w, Gaussian m s)) ->
              VU.zipWith3 (\xi mi si -> a * (xi - mi) / sqrt si / sqrt w) x m s)
          assignment .
        model $
        gmm)
    assignments
    xs
  where
    assignments = getAssignmentVec gmm xs

fisherVectorSigma :: GMM
                  -> [VU.Vector Double]
                  -> VU.Vector Double
fisherVectorSigma gmm xs =
  VU.map (/ (sqrt . (*) 2 . fromIntegral . L.length $ xs)) .
  L.foldl1' (VU.zipWith (+)) $
  L.zipWith
    (\assignment x ->
        VU.concat .
        L.zipWith
          (\a (Model (w, Gaussian m s)) ->
              VU.zipWith3
                (\xi mi si -> a * ((xi - mi) ^ (2 :: Int) / si - 1) / sqrt w)
                x
                m
                s)
          assignment .
        model $
        gmm)
    assignments
    xs
  where
    assignments = getAssignmentVec gmm xs

fisherVectorConduit
  :: ParallelParams
  -> GMM
  -> Conduit (Int,[VU.Vector Double]) (ResourceT IO) (Int,VU.Vector Double)
fisherVectorConduit parallelParams gmm = do
  xs <- CL.take (batchSize parallelParams)
  unless
    (L.null xs)
    (do let !ys =
              parMapChunk
                parallelParams
                rdeepseq
                (\(label, x) ->
                    let !vecMu = fisherVectorMu gmm x
                        !vecSigma = fisherVectorSigma gmm x
                        !vec = vecMu VU.++ vecSigma
                        !powerVec =
                          VU.map (\x' -> signum x' * (abs x' ** 0.5)) vec
                        !l2Norm =
                          sqrt
                            (VU.foldl' (\a b -> a + b ^ (2 :: Int)) 0 powerVec)
                        !result =
                          if l2Norm == 0
                            then VU.replicate (VU.length vec) 0
                            else VU.map (/ l2Norm) powerVec
                    in (label, result))
                xs
        sourceList ys
        fisherVectorConduit parallelParams gmm)

fisherVectorMultilayerConduit
  :: ParallelParams
  -> [GMM]
  -> Conduit (Double, [[VU.Vector Double]]) (ResourceT IO) (Double, VU.Vector Double)
fisherVectorMultilayerConduit parallelParams gmms = do
  xs <- CL.take (batchSize parallelParams)
  unless
    (L.null xs)
    (do let !ys =
              parMapChunk
                parallelParams
                rdeepseq
                (\(label, zs) ->
                   let !vecMus = L.zipWith fisherVectorMu gmms zs
                       !vecSigmas = L.zipWith fisherVectorSigma gmms zs
                       !vecs = L.zipWith (VU.++) vecMus vecSigmas
                       !powerVecs =
                         L.map (VU.map (\x' -> signum x' * sqrt (abs x'))) $
                         vecs
                       !l2Norms =
                         L.map (sqrt . VU.foldl' (\a b -> a + b ^ (2 :: Int)) 0) $
                         powerVecs
                       !result =
                         VU.concat $
                         L.zipWith
                           (\l2Norm powerVec ->
                              if l2Norm == 0
                                then VU.replicate (VU.length powerVec) 0
                                else VU.map (/ l2Norm) powerVec)
                           l2Norms
                           powerVecs
                   in (label, result))
                xs
        sourceList ys
        fisherVectorMultilayerConduit parallelParams gmms)
