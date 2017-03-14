{-# LANGUAGE BangPatterns #-}
module Application.FacialExpression.PCA where

import           Classifier.LibLinear
import           Control.Arrow
import           Control.Monad              as M
import           Data.Array
import           Data.List                  as L
import           Data.Vector.Unboxed        as VU
import           Numeric.LinearAlgebra.Data as LA
import           Numeric.Statistics.PCA

{-# INLINE computePCAMatrix #-}

computePCAMatrix :: Int -> [VU.Vector Double] -> Matrix Double
computePCAMatrix numPrincipal xs = snd $ pcaN arr' numPrincipal
  where
    arr' = listArray (1, L.length xs) . L.map (LA.fromList . VU.toList) $ xs

{-# INLINE computePCA #-}

computePCA :: Matrix Double
    -> [VU.Vector Double]
    -> [VU.Vector Double]
computePCA pcaMatrix xs =
  L.map (VU.fromList . LA.toList) .
  elems .
  pcaTransform
    (listArray (1, L.length xs) . L.map (LA.fromList . VU.toList) $ xs) $
  pcaMatrix

crossValidation :: TrainParams
                -> Int
                -> Int
                -> [(Double, VU.Vector Double)]
                -> IO ()
crossValidation (TrainParams solver c _ maxIndex' _modelName) nFold numPrincipal trainLabelFeature = do
  let labelFeatureList = splitList (div (L.length trainLabelFeature) nFold) trainLabelFeature
  percent <-
    M.mapM
      (\i ->
          let (as, bs) = L.splitAt i labelFeatureList
              trainInstances = L.concat $! as L.++ L.tail bs
              testInstances = L.head bs
              pcaMatrix = computePCAMatrix numPrincipal . snd . L.unzip $ trainInstances
              reducedTrainInstances = computePCA pcaMatrix . snd . L.unzip $ trainInstances
              reducedTestInstances = computePCA pcaMatrix . snd . L.unzip $ testInstances
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
