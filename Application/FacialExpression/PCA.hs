{-# LANGUAGE BangPatterns #-}
module Application.FacialExpression.PCA where

import           Classifier.LibLinear
import           Control.Arrow
import           Control.Monad        as M
import           CV.Statistics.PCA
import           CV.Utility.Parallel
import           Data.Array
import           Data.List            as L
import           Data.Vector.Unboxed  as VU

-- {-# INLINE computePCAMatrix #-}

-- computePCAMatrix :: Int -> [VU.Vector Double] -> Matrix Double
-- computePCAMatrix numPrincipal xs = pcaN arr' numPrincipal
--   where
--     ys = L.transpose . L.map VU.toList $ xs
--     arr' = listArray (1, L.length ys) . L.map (LA.fromList) $ ys

-- {-# INLINE computePCA #-}

-- computePCA :: Matrix Double
--     -> [VU.Vector Double]
--     -> [VU.Vector Double]
-- computePCA pcaMatrix xs =
--   L.map VU.fromList .
--   L.transpose .
--   L.map LA.toList .
--   elems . pcaTransform (listArray (1,L.length ys) . L.map LA.fromList $ ys) $
--   pcaMatrix
--   where ys = L.transpose . L.map VU.toList $ xs

crossValidation :: ParallelParams
                -> TrainParams
                -> Int
                -> [(Double,VU.Vector Double)]
                -> IO ()
crossValidation parallelParams (TrainParams solver c _ maxIndex' _modelName) nFold trainLabelFeature =
  do let labelFeatureList =
           splitList (div (L.length trainLabelFeature) nFold) trainLabelFeature
     percent <-
       M.mapM (\i ->
                 let (as,bs) = L.splitAt i labelFeatureList
                     trainInstances = L.concat $! as L.++ L.tail bs
                     testInstances = L.head bs
                 in trainNPredict
                      (TrainParams solver
                                   c
                                   (L.length trainInstances)
                                   maxIndex'
                                   _modelName)
                      (L.map (second $ getFeature . Dense . VU.toList) $
                       trainInstances)
                      (L.map (second $ getFeature . Dense . VU.toList) $
                       testInstances))
              [0 .. nFold - 1]
     putStrLn $ show (L.sum percent / fromIntegral nFold * 100) L.++ "%"
  where splitList _ [] = []
        splitList n !xs = as : splitList n bs
          where (as,bs) = L.splitAt n xs
        

crossValidationPCA :: ParallelParams
                   -> TrainParams
                   -> Int
                   -> Int
                   -> [(Double,VU.Vector Double)]
                   -> IO ()
crossValidationPCA parallelParams (TrainParams solver c _ maxIndex' _modelName) nFold numPrincipal trainLabelFeature = do
  let labelFeatureList = splitList (div (L.length trainLabelFeature) nFold) trainLabelFeature
  percent <-
    M.mapM
      (\i ->
          let (as, bs) = L.splitAt i labelFeatureList
              trainInstances = L.concat $! as L.++ L.tail bs
              testInstances = L.head bs
              (pcaMatrix, reducedTrainInstances) = pcaSVD parallelParams numPrincipal . snd . L.unzip $ trainInstances
              reducedTestInstances = pcaReduction parallelParams pcaMatrix . snd . L.unzip $ testInstances
          in trainNPredict
               (TrainParams solver c (L.length trainInstances) maxIndex' _modelName)
               (L.zip (fst . L.unzip $ trainInstances) .
                L.map (getFeature . Dense . VU.toList) $
                reducedTrainInstances)
               (L.zip (fst . L.unzip $ testInstances) .
                L.map (getFeature . Dense . VU.toList) $
                reducedTestInstances))
      [0 .. nFold - 1]
  putStrLn $ show (L.sum percent / fromIntegral nFold * 100) L.++ "%"
  where
    splitList _ [] = []
    splitList n !xs = as : splitList n bs
      where
        (as, bs) = L.splitAt n xs

{-# INLINE normalizeVec #-}

normalizeVec :: VU.Vector Double -> VU.Vector Double
normalizeVec vec = VU.map (/s) vec
  where s = sqrt . VU.sum . VU.map (^2) $ vec


