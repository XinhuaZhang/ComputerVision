{-# LANGUAGE BangPatterns #-}
module Application.GMM.GMM
  (assignGMM
  ,getNK
  ,updateMuGMM
  ,updateSigmaGMM
  ,updateWGMM
  ,gmmSink)
  where

import           Application.GMM.Gaussian
import           Application.GMM.MixtureModel
import           Control.Monad.IO.Class
import           CV.Utility.Parallel
import           Data.Binary
import           Data.Conduit
import           Data.Conduit.List            as CL
import           Data.Vector                  as V
import           Data.Vector.Unboxed          as VU
import           GHC.Generics
import           Prelude                      as P
import           System.Random

type GMM = MixtureModel Gaussian
type GMMData = VU.Vector Double
type GMMParameters = VU.Vector Double
type Assignment = V.Vector Double

assignGMM
  :: ParallelParams -> GMM -> V.Vector GMMData -> (V.Vector Assignment, V.Vector Double)
assignGMM parallelParams (MixtureModel n modelVec) xs =
  (parZipWithChunkVector
     parallelParams
     rdeepseq
     (\x z -> V.map (\(Model (wk,mk)) -> (wk * gaussian mk x) / z) modelVec)
     xs
     zs
  ,zs)
  where !zs =
          parMapChunkVector
            parallelParams
            rdeepseq
            (\x ->
               V.foldl' (\v (Model (wj,mj)) -> v + (wj * gaussian mj x)) 0 modelVec)
            xs

getNK :: V.Vector Assignment -> V.Vector Double
getNK = V.foldl1' (V.zipWith (+))


updateMuKGMM
  :: V.Vector GMMData -> Assignment -> Double -> GMMParameters
updateMuKGMM xs assignment nk =
  VU.map (/ nk) .
  (V.foldl1' (VU.zipWith (+)) . V.zipWith (\a x -> VU.map (* a) x) assignment) $
  xs

updateMuGMM :: ParallelParams
            -> V.Vector GMMData
            -> V.Vector Assignment
            -> V.Vector Double
            -> V.Vector GMMParameters
updateMuGMM parallelParams xs assignments nks =
  parZipWithChunkVector parallelParams
                        rdeepseq
                        (updateMuKGMM xs)
                        assignments
                        nks

updateSigmaKGMM :: V.Vector GMMData
                -> Assignment
                -> Double
                -> GMMParameters
                -> GMMParameters
updateSigmaKGMM xs assignment nk newMuK =
  VU.map (/ nk) .
  (V.foldl1' (VU.zipWith (+)) .
   V.zipWith (\a x ->
                VU.map (* a) .
                VU.zipWith (\mu y -> (y - mu) ^ 2)
                           newMuK $
                x)
             assignment) $
  xs

updateSigmaGMM :: ParallelParams
               -> V.Vector GMMData
               -> V.Vector Assignment
               -> V.Vector Double
               -> V.Vector GMMParameters
               -> V.Vector GMMParameters
updateSigmaGMM parallelParams xs assignments nks newMu =
  parZipWith3ChunkVector parallelParams
                         rdeepseq
                         (updateSigmaKGMM xs)
                         assignments
                         nks
                         newMu

updateWGMM
  :: Int -> V.Vector Double -> V.Vector Double
updateWGMM n = V.map (/ fromIntegral n)


getLiklihood :: V.Vector Double -> Double
getLiklihood = V.foldl' (\a b -> a + log b) 0


-- EM algorithm
em
  :: ParallelParams -> V.Vector GMMData -> GMM -> Double -> GMM
em parallelParams xs models threshold
  | getLiklihood zs > threshold = models
  | otherwise =
    MixtureModel (numModel models) $
    V.zipWith3
      (\w mu sigma ->
         Model (w
               ,Gaussian (numDims . snd . (\(Model x) -> x) . V.head . model $
                          models)
                         mu
                         sigma))
      newW
      newMu
      newSigma
  where (assignments,zs) = assignGMM parallelParams models xs
        nks = getNK assignments
        newMu = updateMuGMM parallelParams xs assignments nks
        newSigma = updateSigmaGMM parallelParams xs assignments nks newMu
        newW =
          updateWGMM (V.length xs)
                     nks


initializeGMM :: Int -> Int -> IO GMM
initializeGMM numModel numDimension =
  do gen <- getStdGen
     let mu = randomRs (-1,1)
         sigma = randomRs (0.1,1)
         w' = P.take numModel $ randoms gen :: [Int]
         ws' = P.fromIntegral . P.sum $ w'
         w = V.fromList $ P.map (\x -> P.fromIntegral x / ws') w'
         models' =
           V.replicate numModel .
           (\g ->
               (Gaussian numDimension
                              (VU.fromList . P.take numDimension $ mu g)
                              (VU.fromList . P.take numDimension $ sigma g))) $
           gen
         models = V.zipWith (\a b -> Model (a,b)) w models'
     return (MixtureModel numModel models)


gmmSink :: ParallelParams -> Int
        -> Double
        -> FilePath
        -> Sink (V.Vector GMMData) IO ()
gmmSink parallelParams numM threshold filePath =
  do xs <- consume
     models <-
       liftIO $
       initializeGMM numM
                     (VU.length . V.head . P.head $ xs)
     let ys = V.concat xs
         trainedModel = em parallelParams ys models threshold
     liftIO $ encodeFile filePath trainedModel
