{-# LANGUAGE BangPatterns #-}
module Application.MultiDimensionalGMM.FisherKernel where

import           Application.MultiDimensionalGMM.Gaussian
import           Application.MultiDimensionalGMM.GMM
import           Application.MultiDimensionalGMM.MixtureModel
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           CV.Utility.Parallel
import           Data.Conduit
import           Data.Conduit.List                            as CL
import           Data.List                                    as L
import           Data.Vector                                  as V
import           Data.Vector.Unboxed                          as VU

fisherVectorMu :: ParallelParams -> GMM -> [VU.Vector Double] -> VU.Vector Double
fisherVectorMu parallelParams gmm xs =
  VU.map (/ (sqrt . fromIntegral . L.length $ xs)) . L.foldl1' (VU.zipWith (+)) $
  parZipWithChunk
    parallelParams
    rdeepseq
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

fisherVectorSigma :: ParallelParams
                  -> GMM
                  -> [VU.Vector Double]
                  -> VU.Vector Double
fisherVectorSigma parallelParams gmm xs =
  VU.map (/ (sqrt . (*) 2 . fromIntegral . L.length $ xs)) .
  L.foldl1' (VU.zipWith (+)) $
  parZipWithChunk
    parallelParams
    rdeepseq
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
fisherVectorConduit parallelParams gmm =
  awaitForever
    (\(label, x) ->
        let !vecMu = fisherVectorMu parallelParams gmm x
            !vecSigma = fisherVectorSigma parallelParams gmm x
            !vec = vecMu VU.++ vecSigma
            !powerVec = VU.map (\x' -> signum x' * ((abs x') ** 0.5)) vec
            !l2Norm = sqrt (VU.foldl' (\a b -> a + b ^ (2 :: Int)) 0 powerVec)
            !result =
              if l2Norm == 0
                then powerVec
                else VU.map (/ l2Norm) powerVec
        in yield (label, result))
