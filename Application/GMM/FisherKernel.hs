{-# LANGUAGE BangPatterns #-}
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

fisherVectorMu :: ParallelParams -> [GMM] -> [AssignmentVec] -> [VU.Vector Double] -> VU.Vector Double
fisherVectorMu parallelParams gmms assignments xs =
  VU.concat $
  parZipWith3Chunk
    parallelParams
    rdeepseq
    (\(MixtureModel _ modelVec) assignment x ->
       let !muVec =
             V.convert . V.map (\(Model m) -> gaussianMu . snd $ m) $ modelVec
           !sigmaVec =
             V.convert . V.map (\(Model m) -> gaussianSigma . snd $ m) $
             modelVec
           !wVec = V.convert . V.map (\(Model m) -> fst m) $ modelVec
       in VU.zipWith3
            (\wk sigma' y ->
               y * sqrt (sigma' / (fromIntegral (VU.length x) * wk)))
            wVec
            sigmaVec .
          V.foldl1' (VU.zipWith (+)) .
          V.zipWith (\an xn ->
                       VU.zipWith3 (\mu' sigma' a -> a * (xn - mu') / sigma')
                                   muVec
                                   sigmaVec
                                   an)
                    assignment .
          VU.convert $
          x)
    gmms
    assignments
    xs

fisherVectorSigma :: ParallelParams
                  -> [GMM]
                  -> [AssignmentVec]
                  -> [VU.Vector Double]
                  -> VU.Vector Double
fisherVectorSigma parallelParams gmms assignments xs =
  VU.concat $
  parZipWith3Chunk
    parallelParams
    rdeepseq
    (\(MixtureModel _ modelVec) assignment x ->
       let !muVec =
             V.convert . V.map (\(Model m) -> gaussianMu . snd $ m) $ modelVec
           !sigmaVec =
             V.convert . V.map (\(Model m) -> gaussianSigma . snd $ m) $
             modelVec
           !wVec = V.convert . V.map (\(Model m) -> fst m) $ modelVec
       in VU.zipWith
            (\wk y -> y * sqrt (0.5 / (fromIntegral (VU.length x) * wk)))
            wVec .
          V.foldl1' (VU.zipWith (+)) .
          V.zipWith (\an xn ->
                       VU.zipWith3
                         (\mu' sigma' a ->
                            a * ((xn - mu') ^ (2 :: Int) / sigma' - 1))
                         muVec
                         sigmaVec
                         an)
                    assignment .
          VU.convert $
          x)
    gmms
    assignments
    xs

fisherVectorConduit
  :: ParallelParams
  -> [GMM]
  -> Conduit (Int,[VU.Vector Double]) IO (Int,VU.Vector Double)
fisherVectorConduit parallelParams gmms =
  awaitForever
    (\(label,x) ->
       let !assignments =
             parZipWithChunk parallelParams rdeepseq getAssignmentVecSafe gmms x
           !vecMu = fisherVectorMu parallelParams gmms assignments x
           !vecSigma = fisherVectorSigma parallelParams gmms assignments x
           !vec = vecMu VU.++ vecSigma
           !l2Norm = sqrt (VU.foldl' (\a b -> a + b ^ (2 :: Int)) 0 vec)
       in yield (label,VU.map (/ l2Norm) vec))

