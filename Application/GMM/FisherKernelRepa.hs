{-# LANGUAGE FlexibleContexts #-}

module Application.GMM.FisherKernelRepa where

import           Application.GMM.Gaussian
import           Application.GMM.GMM
import           Application.GMM.MixtureModel
import           Control.Monad.IO.Class
import           CV.Utility.Parallel
import           Data.Array.Repa              as R
import           Data.Conduit
import           Data.Conduit.List            as CL
import           Data.List                    as L
import           Data.Vector                  as V
import           Data.Vector.Unboxed          as VU
import           Prelude                      as P
import Control.Monad

-- chw -> hwc
rotateArr
  :: R.Source s Double
  => R.Array s DIM3 Double -> R.Array D DIM3 Double
rotateArr arr =
  R.backpermute
    (Z :. ny :. nx :. nf)
    (\(Z :. h :. w :. c) -> (Z :. c :. h :. w))
    arr
  where
    (Z :. nf :. ny :. nx) = extent arr

computeAssignmentP
  :: (R.Source s Double)
  => GMM -> R.Array s DIM3 Double -> IO (R.Array U DIM3 Double)
computeAssignmentP gmm@(MixtureModel n modelVec) arr = do
  khwArr <- sumP khwcArr
  let weightedProbArr =
        R.traverse
          khwArr
          id
          (\f idx@(Z :. k :. h :. w) ->
              let (Model (a, Gaussian _ mu' sigma')) = modelVec V.! k
              in a * (exp (-0.5 * f idx)) / (VU.product sigma'))
  zArr <- sumP . rotateArr $ weightedProbArr
  computeUnboxedP $
    R.traverse2
      khwArr
      zArr
      const
      (\f1 f2 idx@(Z :. _ :. h :. w) -> f1 idx / (f2 (Z :. h :. w)))
  where
    khwcArr =
      R.fromFunction
        (Z :. n :. ny :. nx :. nf)
        (\(Z :. k :. h :. w :. c) ->
            let (Model (_, Gaussian _ mu' sigma')) = modelVec V.! k
                a = arr R.! (Z :. c :. h :. w)
            in ((a - mu' VU.! c) / (sigma' VU.! c)) ^ 2)
    (Z :. nf :. ny :. nx) = extent arr

fisherVectorMuP
  :: (R.Source s Double)
  => GMM
  -> R.Array U DIM3 Double
  -> R.Array s DIM3 Double
  -> IO (VU.Vector Double)
fisherVectorMuP gmm@(MixtureModel n modelVec) assignment arr = do
  let fisherDerivativeMu =
        R.traverse2
          kchwArr
          assignment
          const
          (\f1 f2 idx@(Z :. k :. c :. h :. w) ->
              let (Model (_, Gaussian _ mu' sigma')) = modelVec V.! k
              in f2 (Z :. k :. h :. w) * (f1 idx - mu' VU.! c) / (sigma' VU.! c))
  s1 <- sumP fisherDerivativeMu
  s <- sumP s1
  result <-
    computeP $
    R.traverse
      s
      id
      (\f idx@(Z :. k :. c) ->
          let (Model (w, _)) = modelVec V.! k
          in f idx / sqrt (fromIntegral (nx * ny) * w))
  return . toUnboxed $ result
  where
    kchwArr =
      R.fromFunction
        (Z :. n :. nf :. ny :. nx)
        (\(Z :. k :. c :. h :. w) ->
            let (Model (_, Gaussian _ mu' sigma')) = modelVec V.! k
                a = arr R.! (Z :. c :. h :. w)
            in ((a - mu' VU.! c) / (sigma' VU.! c)) ^ 2)
    (Z :. nf :. ny :. nx) = extent arr

fisherVectorSigmaP
  :: (R.Source s Double)
  => GMM
  -> R.Array U DIM3 Double
  -> R.Array s DIM3 Double
  -> IO (VU.Vector Double)
fisherVectorSigmaP gmm@(MixtureModel n modelVec) assignment arr = do
  let fisherDerivativeSigma =
        R.traverse2
          kchwArr
          assignment
          const
          (\f1 f2 idx@(Z :. k :. c :. h :. w) ->
              let (Model (_, Gaussian _ mu' sigma')) = modelVec V.! k
              in f2 (Z :. k :. h :. w) *
                 (((f1 idx - mu' VU.! c) / (sigma' VU.! c)) ^ 2 - 1))
  s1 <- sumP $ fisherDerivativeSigma
  s <- sumP s1
  result <-
    computeP $
    R.traverse
      s
      id
      (\f idx@(Z :. k :. c) ->
          let (Model (w, _)) = modelVec V.! k
          in f idx / sqrt (fromIntegral (nx * ny) * w * 2))
  return . toUnboxed $ result
  where
    kchwArr =
      R.fromFunction
        (Z :. n :. nf :. ny :. nx)
        (\(Z :. k :. c :. h :. w) ->
            let (Model (_, Gaussian _ mu' sigma')) = modelVec V.! k
                a = arr R.! (Z :. c :. h :. w)
            in ((a - mu' VU.! c) / (sigma' VU.! c)) ^ 2)
    (Z :. nf :. ny :. nx) = extent arr

fisherVectorConduit
  :: (R.Source s Double)
  => GMM -> Conduit (R.Array s DIM3 Double) IO (VU.Vector Double)
fisherVectorConduit gmm =
  awaitForever
    (\x -> do
       assignment <- liftIO $ computeAssignmentP gmm x
       muVec <- liftIO $ fisherVectorMuP gmm assignment x
       sigmaVec <- liftIO $ fisherVectorSigmaP gmm assignment x
       let vec = muVec VU.++ sigmaVec
           l2Norm = sqrt (VU.foldl' (\a b -> a + b ^ 2) 0 vec)
       yield $ VU.map (/ l2Norm) vec)


--ComputeS 

computeAssignmentS
  :: (R.Source s Double)
  => GMM -> R.Array s DIM3 Double -> R.Array U DIM3 Double
computeAssignmentS gmm@(MixtureModel n modelVec) arr =
  computeUnboxedS $
  R.traverse2
    khwArr
    zArr
    const
    (\f1 f2 idx@(Z :. _ :. h :. w) -> f1 idx / (f2 (Z :. h :. w)))
  where
    khwcArr =
      R.fromFunction
        (Z :. n :. ny :. nx :. nf)
        (\(Z :. k :. h :. w :. c) ->
            let (Model (_, Gaussian _ mu' sigma')) = modelVec V.! k
                a = arr R.! (Z :. c :. h :. w)
            in ((a - mu' VU.! c) / (sigma' VU.! c)) ^ 2)
    (Z :. nf :. ny :. nx) = extent arr
    khwArr = sumS khwcArr
    weightedProbArr =
      R.traverse
        khwArr
        id
        (\f idx@(Z :. k :. h :. w) ->
            let (Model (a, Gaussian _ mu' sigma')) = modelVec V.! k
            in a * (exp (-0.5 * f idx)) / (VU.product sigma'))
    zArr = sumS . rotateArr $ weightedProbArr

fisherVectorMuS
  :: (R.Source s Double)
  => GMM
  -> R.Array U DIM3 Double
  -> R.Array s DIM3 Double
  -> VU.Vector Double
fisherVectorMuS gmm@(MixtureModel n modelVec) assignment arr =
  toUnboxed . computeS $
  R.traverse
    s
    id
    (\f idx@(Z :. k :. c) ->
        let (Model (w, _)) = modelVec V.! k
        in f idx / sqrt (fromIntegral (nx * ny) * w))
  where
    kchwArr =
      R.fromFunction
        (Z :. n :. nf :. ny :. nx)
        (\(Z :. k :. c :. h :. w) ->
            let (Model (_, Gaussian _ mu' sigma')) = modelVec V.! k
                a = arr R.! (Z :. c :. h :. w)
            in ((a - mu' VU.! c) / (sigma' VU.! c)) ^ 2)
    (Z :. nf :. ny :. nx) = extent arr
    fisherDerivativeMu =
      R.traverse2
        kchwArr
        assignment
        const
        (\f1 f2 idx@(Z :. k :. c :. h :. w) ->
            let (Model (_, Gaussian _ mu' sigma')) = modelVec V.! k
            in f2 (Z :. k :. h :. w) * (f1 idx - mu' VU.! c) / (sigma' VU.! c))
    s = sumS . sumS $ fisherDerivativeMu

fisherVectorSigmaS
  :: (R.Source s Double)
  => GMM
  -> R.Array U DIM3 Double
  -> R.Array s DIM3 Double
  -> VU.Vector Double
fisherVectorSigmaS gmm@(MixtureModel n modelVec) assignment arr =
  toUnboxed . computeS $
  R.traverse
    s
    id
    (\f idx@(Z :. k :. c) ->
        let (Model (w, _)) = modelVec V.! k
        in f idx / sqrt (fromIntegral (nx * ny) * w * 2))
  where
    kchwArr =
      R.fromFunction
        (Z :. n :. nf :. ny :. nx)
        (\(Z :. k :. c :. h :. w) ->
            let (Model (_, Gaussian _ mu' sigma')) = modelVec V.! k
                a = arr R.! (Z :. c :. h :. w)
            in ((a - mu' VU.! c) / (sigma' VU.! c)) ^ 2)
    (Z :. nf :. ny :. nx) = extent arr
    fisherDerivativeSigma =
      R.traverse2
        kchwArr
        assignment
        const
        (\f1 f2 idx@(Z :. k :. c :. h :. w) ->
            let (Model (_, Gaussian _ mu' sigma')) = modelVec V.! k
            in f2 (Z :. k :. h :. w) *
               (((f1 idx - mu' VU.! c) / (sigma' VU.! c)) ^ 2 - 1))
    s = sumS . sumS $ fisherDerivativeSigma

fisherVectorConduitS
  :: (R.Source s Double)
  => ParallelParams
  -> GMM
  -> Conduit (R.Array s DIM3 Double) IO (VU.Vector Double)
fisherVectorConduitS parallelParams gmm = do
  xs <- CL.take (batchSize parallelParams)
  unless
    (L.null xs)
    (do let ys =
              parMapChunk
                parallelParams
                rseq
                (\x ->
                    let assignment = computeAssignmentS gmm x
                        muVec = fisherVectorMuS gmm assignment x
                        sigmaVec = fisherVectorSigmaS gmm assignment x
                        vec = muVec VU.++ sigmaVec
                        l2Norm = sqrt (VU.foldl' (\a b -> a + b ^ 2) 0 vec)
                    in VU.map (/ l2Norm) vec)
                xs
        sourceList ys
        fisherVectorConduitS parallelParams gmm)
-- awaitForever
--   (\x -> do
--      assignment <- liftIO $ computeAssignmentP gmm x
--      muVec <- liftIO $ fisherVectorMuP gmm assignment x
--      sigmaVec <- liftIO $ fisherVectorSigmaP gmm assignment x
--      let vec = muVec VU.++ sigmaVec
--          l2Norm = sqrt (VU.foldl' (\a b -> a + b ^ 2) 0 vec)
--      yield $ VU.map (/ l2Norm) vec)
