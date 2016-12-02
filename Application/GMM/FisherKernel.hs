{-# LANGUAGE BangPatterns  #-}
module Application.GMM.FisherKernel where

import           Application.GMM.Gaussian
import           Application.GMM.GMM
import           Application.GMM.MixtureModel
import           Control.Monad
import           CV.Utility.Parallel

import           Data.Conduit
import           Data.Conduit.List            as CL
import           Data.List                    as L

import           Data.Vector                  as V
import           Data.Vector.Unboxed          as VU

fisherVectorMu :: [GMM] -> [AssignmentVec] -> [VU.Vector Double] -> VU.Vector Double
fisherVectorMu gmms assignments xs =
  VU.concat $
  L.zipWith3
    (\(MixtureModel _ modelVec) assignment x ->
        V.convert $
        V.zipWith
          (\(Model (wk, Gaussian mu' sigma')) a ->
              sqrt (sigma' / (fromIntegral (VU.length x) * wk)) *
              VU.sum (VU.zipWith (\an xn -> an * (xn - mu') / sigma') a x))
          modelVec
          assignment)
    gmms
    assignments
    xs

fisherVectorSigma :: [GMM]
                  -> [AssignmentVec]
                  -> [VU.Vector Double]
                  -> VU.Vector Double
fisherVectorSigma gmms assignments xs =
  VU.concat $
  L.zipWith3
    (\(MixtureModel _ modelVec) assignment x ->
        V.convert $
        V.zipWith
          (\(Model (wk, Gaussian mu' sigma')) a ->
              sqrt (0.5 / (fromIntegral (VU.length x) * wk)) *
              VU.sum
                (VU.zipWith
                   (\an xn -> an * ((xn - mu') ^ (2 :: Int) / sigma' - 1))
                   a
                   x))
          modelVec
          assignment)
    gmms
    assignments
    xs

fisherVectorConduit
  :: ParallelParams
  -> [GMM]
  -> Conduit (Int, [VU.Vector Double]) IO (Int, VU.Vector Double)
fisherVectorConduit parallelParams gmms = do
  xs <- CL.take (batchSize parallelParams)
  unless
    (L.null xs)
    (do let ys =
              parMapChunk
                parallelParams
                rdeepseq
                (\(label, x) ->
                    let !assignments = L.zipWith getAssignmentVec gmms x
                        !vecMu = fisherVectorMu gmms assignments x
                        !vecSigma = fisherVectorSigma gmms assignments x
                        !vec = vecMu VU.++ vecSigma
                        !l2Norm =
                          sqrt (VU.foldl' (\a b -> a + b ^ (2 :: Int)) 0 vec)
                    in (label, VU.map (/ l2Norm) vec))
                xs
        sourceList ys
        fisherVectorConduit parallelParams gmms)
