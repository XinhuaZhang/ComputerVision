{-# LANGUAGE BangPatterns #-}

module Application.GMM.GMM
       (assignGMM, getNK, updateMuGMM, updateSigmaGMM, updateWGMM,
        gmmTestSink, gmmSink)
       where

import           Application.GMM.Gaussian
import           Application.GMM.MixtureModel
import           Control.DeepSeq              as DS
import           Control.Monad                as M
import           Control.Monad.IO.Class
import           Control.Parallel
import           CV.Utility.Parallel
import           Data.Binary
import           Data.Conduit
import           Data.Conduit.List            as CL
import           Data.Maybe
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
  :: ParallelParams
  -> GMM
  -> V.Vector GMMData
  -> (V.Vector Assignment,Double)
assignGMM parallelParams (MixtureModel n modelVec) xs =
  zs `pseq` likelihood `par` assignments `pseq` (assignments,likelihood)
  where !zs =
          parMapChunkVector
            parallelParams
            rdeepseq
            (\x ->
               V.sum $
               V.map (\(Model (wj,mj)) -> (wj * gaussian mj x)) modelVec)
            xs
        assignments =
          parMapChunkVector
            parallelParams
            rdeepseq
            (\(Model (wk,mk)) ->
               V.zipWith (\x z -> (wk * gaussian mk x) / z) xs zs)
            modelVec
        likelihood = getLikelihood zs


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
updateSigmaKGMM xs assignment nk newMuK
  | VU.or . VU.map (== 0) $ newSigma =
    VU.map (\x ->
              if x == 0
                 then 10 ** (-3)
                 else x)
           newSigma
  | otherwise = newSigma
  where newSigma =
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

getLikelihood :: V.Vector Double -> Double
getLikelihood = V.foldl' (\a b -> a + log b) 0

getNK :: V.Vector Assignment -> V.Vector Double
getNK = V.map (V.foldl1' (+))

-- EM algorithm
emTest :: ParallelParams
       -> V.Vector GMMData
       -> Double
       -> GMM
       -> IO (GMM
             ,Double
             ,Assignment
             ,V.Vector Double
             ,V.Vector GMMParameters
             ,V.Vector GMMParameters
             ,V.Vector Double)
emTest parallelParams xs threshold models
  | V.or . V.map (<= 0) $ nks =
    error "nk is zero! Try increasing the initialization range of sigma and decreasing that of mu."
  | isNaN likelihood =
    error "Try increasing the initialization range of sigma and decreasing that of mu."
  | otherwise =
    return (newModel
           ,likelihood
           ,V.map V.head assignments
           ,nks
           ,newMu
           ,newSigma
           ,newW)
  where (assignments,likelihood) = assignGMM parallelParams models xs
        !nks = getNK assignments
        newMu = updateMuGMM parallelParams xs assignments nks
        newSigma = updateSigmaGMM parallelParams xs assignments nks newMu
        !newW =
          updateWGMM (V.length xs)
                     nks
        !newModel =
          newMu `pseq`
          newSigma `pseq`
          MixtureModel (numModel models) $
          V.zipWith3
            (\w mu sigma ->
               Model (w
                     ,Gaussian (numDims .
                                snd . (\(Model x) -> x) . V.head . model $
                                models)
                               mu
                               sigma))
            newW
            newMu
            newSigma

em
  :: ParallelParams -> FilePath -> V.Vector GMMData  -> Double -> Double -> GMM -> IO ()
em parallelParams filePath xs threshold oldLikelihood oldModel
  | isNaN newLikelihood =
    error "Try increasing the initialization range of sigma and decreasing that of mu."
  | abs ((oldLikelihood - newLikelihood) / oldLikelihood * 100) < threshold =
    liftIO $ encodeFile filePath newModel
  | otherwise =
    do putStrLn $
         (show newLikelihood) P.++ " (" P.++
         (show $ abs $ (newLikelihood - oldLikelihood) / oldLikelihood * 100) P.++
         "%)"
       liftIO $ encodeFile filePath newModel
       em parallelParams filePath xs threshold newLikelihood newModel
  where (assignments,newLikelihood) = assignGMM parallelParams oldModel xs
        !nks = getNK assignments
        newMu = updateMuGMM parallelParams xs assignments nks
        newSigma = updateSigmaGMM parallelParams xs assignments nks newMu
        !newW =
          updateWGMM (V.length xs)
                     nks
        !newModel =
          newW `par`
          newMu `pseq`
          MixtureModel (numModel oldModel) $
          V.zipWith3
            (\w mu sigma ->
               Model (w
                     ,Gaussian (numDims .
                                snd . (\(Model x) -> x) . V.head . model $
                                oldModel)
                               mu
                               sigma))
            newW
            newMu
            newSigma

initializeGMM :: Int -> Int -> IO GMM
initializeGMM numModel numDimension =
  do gen <- getStdGen
     let (w',gen1) =
           randomRList numModel
                       (1,100)
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
                      (-10,10)
                      gen
        (sigma,newGen2) =
          randomRList numDimension
                      (100,1000)
                      newGen1

gmmTestSink :: ParallelParams
            -> Int
            -> Double
            -> FilePath
            -> Sink (V.Vector GMMData) IO ()
gmmTestSink parallelParams numM threshold filePath =
  do xs <- consume
     models <-
       liftIO $
       initializeGMM numM
                     (VU.length . V.head . P.head $ xs)
     let !ys = V.concat xs
     trainedModel <-
       liftIO $
       M.foldM (\(oldLike,oldModel) b ->
                  do (newModel,like,assignment,nks,newMu,newSigma,newW) <-
                       emTest parallelParams ys threshold oldModel
                     putStrLn $ (show (b + 1)) P.++ ":"
                     putStrLn $
                       "likelihood: " P.++ (show like) P.++ " (" P.++
                       (show $ abs $ (like - oldLike) / oldLike * 100) P.++
                       "%)"
                     -- putStrLn $ "assignment: " P.++ show (V.take 5 assignment)
                     -- putStrLn $ "zs: " P.++ show (V.take 5 zs)
                     -- putStrLn $ "nks: " P.++ show (V.take 5 nks)
                     -- putStrLn $ "newMu: " P.++ show (V.head newMu)
                     -- putStrLn $ "newSigma: " P.++ show (V.head newSigma)
                     -- putStrLn $ "newW: " P.++ show (V.take 5 newW)
                     return (like,newModel))
               (0,models)
               (V.generate 10 id)
     liftIO $ encodeFile filePath trainedModel


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
     let !ys = V.concat xs
     liftIO $ em parallelParams filePath ys threshold 0 models
