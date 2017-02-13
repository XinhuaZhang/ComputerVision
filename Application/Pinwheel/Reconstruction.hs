{-# LANGUAGE FlexibleContexts #-}
module Application.Pinwheel.Reconstruction where

import           Control.Monad               as M
import           CV.Utility.RepaArrayUtility
import           Data.Array.Repa             as R
import           Data.List                   as L
import           Data.Vector.Unboxed         as VU
import           System.Random

computeActivity
  :: (R.Source s Double)
  => Double -> Array s DIM3 Double -> Array U DIM3 Double -> IO [Double]
computeActivity learningRate filteredArr img = do
  act <- M.replicateM filterNf $ randomRIO (-1, 1)
  gradientDecent 100 learningRate (toUnboxed img) featureMapList act
  where
    (Z :. imageNf :. _ :. _) = extent img
    featureMapList =
      L.map (VU.fromList . L.concat) . splitList imageNf . extractFeatureMap $
      filteredArr
    filterNf = L.length featureMapList
    

computeRecon
  :: (R.Source s Double)
  => DIM3 -> Array s DIM3 Double -> [Double] -> Array U DIM3 Double
computeRecon imgExtent filteredArr act =
  fromListUnboxed imgExtent . L.concat $
  L.zipWith (\f a -> L.map (* a) f) featureMapList act
  where
    (Z :. imageNf :. _ :. _) = imgExtent
    featureMapList = L.map L.concat . splitList imageNf . extractFeatureMap $ filteredArr

{-# INLINE gradientDecent #-}

gradientDecent
  :: Int
  -> Double
  -> VU.Vector Double
  -> [VU.Vector Double]
  -> [Double]
  -> IO [Double]
gradientDecent count learningRate img projections activities
  | count == 0 = return activities
  | otherwise = do
    print error'
    gradientDecent
      (count - 1)
      learningRate
      img
      projections
      (L.zipWith (\a d -> a + learningRate * d) activities delta)
  where
    error' =
      VU.sum . VU.zipWith (-) img . L.foldl1' (VU.zipWith (+)) $
      L.zipWith (\p a -> VU.map (* a) p) projections activities
    delta = L.map (\p -> error' * VU.sum p) projections



{-# INLINE splitList #-}

splitList :: Int -> [a] -> [[a]]
splitList _ [] = []
splitList n xs = as : splitList n bs
  where
    (as, bs) = L.splitAt n xs
