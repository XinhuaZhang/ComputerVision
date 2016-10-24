{-# LANGUAGE BangPatterns #-}

module Application.GMM.GMM
       (assignGMM, getNK, updateMuGMM, updateSigmaGMM, updateWGMM,
        gmmSink)
       where

import Application.GMM.Gaussian
import Application.GMM.MixtureModel
import Control.Monad.IO.Class
import Control.Parallel
import CV.Utility.Parallel
import Data.Binary
import Data.Conduit
import Data.Conduit.List as CL
import Data.Maybe
import Data.Vector as V
import Data.Vector.Unboxed as VU
import GHC.Generics
import Prelude as P
import System.Random

type GMM = MixtureModel Gaussian

type GMMData = VU.Vector Double

type GMMParameters = VU.Vector Double

type Assignment = V.Vector Double

assignGMM
  :: ParallelParams
  -> GMM
  -> V.Vector GMMData
  -> (V.Vector Assignment,V.Vector Double)
assignGMM parallelParams (MixtureModel n modelVec) xs = (assignments,zs)
  where zs =
          parMapChunkVector
            parallelParams
            rdeepseq
            (\x ->
               V.foldl' (\v (Model (wj,mj)) -> v + (wj * gaussian mj x)) 0 modelVec)
            xs
        assignments =
          parMapVector
            rdeepseq
            (\(Model (wk,mk)) ->
               V.zipWith (\x z -> (wk * gaussian mk x) / z) xs zs)
            modelVec

getNK :: V.Vector Assignment -> V.Vector Double
getNK = V.map (V.foldl1' (+))

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
  parZipWithVector rdeepseq
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
  parZipWith3Vector rdeepseq
                    (updateSigmaKGMM xs)
                    assignments
                    nks
                    newMu

updateWGMM
  :: Int -> V.Vector Double -> V.Vector Double
updateWGMM n = V.map (/ fromIntegral n)

getLikelihood :: V.Vector Double -> Double
getLikelihood = V.foldl' (\a b -> a + log b) 0

-- EM algorithm
em
  :: ParallelParams -> V.Vector GMMData -> Double -> GMM -> GMM
em parallelParams xs threshold models
  | newW `par` likelihood `pseq` (likelihood > threshold) = models
  | otherwise =
    --   em parallelParams xs threshold $
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
        likelihood = getLikelihood zs
        nks = getNK assignments
        newMu = updateMuGMM parallelParams xs assignments nks
        newSigma = updateSigmaGMM parallelParams xs assignments nks newMu
        !newW =
          updateWGMM (V.length xs)
                     nks

initializeGMM :: Int -> Int -> IO GMM
initializeGMM numModel numDimension =
  do gen <- getStdGen
     let (w',gen1) =
           randomRList numModel
                       (1,10)
                       gen
         ws' = P.sum $ w'
         w = V.fromList $ P.map (/ ws') w'
         models' =
           V.unfoldrN numModel
                      (\g -> Just $ randomGaussian numDimension g)
                      gen1
         models = V.zipWith (\a b -> Model (a,b)) w models'
     return (MixtureModel numModel models)

randomRList :: (RandomGen g,Random a)
            => Int -> (a,a) -> g -> ([a],g)
randomRList len bound gen
  | len > 0 =
    (\(xs,g) -> (x : xs,g)) $
    randomRList (len - 1)
                bound
                newGen
  | otherwise = ([],gen)
  where (x,newGen) = randomR bound gen

randomGaussian :: (RandomGen g)
               => Int -> g -> (Gaussian,g)
randomGaussian numDimension gen =
  (Gaussian numDimension
            (VU.fromList mu)
            (VU.fromList sigma)
  ,newGen2)
  where (mu,newGen1) =
          randomRList numDimension
                      (-1,1)
                      gen
        (sigma,newGen2) =
          randomRList numDimension
                      (0.1,1)
                      newGen1

gmmSink :: ParallelParams
        -> Int
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
         trainedModel =
           ys `par` models `pseq` em parallelParams ys threshold models
     liftIO $ encodeFile filePath trainedModel
