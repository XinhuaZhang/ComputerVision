module CV.Utility.Utility where

import           Data.List           as L
import           Data.Vector.Unboxed as VU
import           Data.Vector.Storable as VS

{-# INLINE rescale #-}

rescale :: (Double,Double) -> [Double] -> [Double]
rescale (lb, ub) xs
  | L.all (== 0) xs = xs
  | otherwise = L.map (\x -> (x - minV) / (maxV - minV) * (ub - lb) + lb) xs
  where
    minV = L.minimum xs
    maxV = L.maximum xs


{-# INLINE rescaleUnboxedVector #-}

rescaleUnboxedVector :: (Double, Double) -> VU.Vector Double -> VU.Vector Double
rescaleUnboxedVector (lb, ub) xs
  | VU.all (== 0) xs = xs
  | otherwise = VU.map (\x -> (x - minV) / (maxV - minV) * (ub - lb) + lb) xs
  where
    minV = VU.minimum xs
    maxV = VU.maximum xs
    

{-# INLINE rescaleUnboxedVectorList #-}

rescaleUnboxedVectorList :: (Double, Double) -> [VU.Vector Double] -> [VU.Vector Double]
rescaleUnboxedVectorList (lb, ub) xs
  | minV == maxV = xs
  | otherwise =
    L.map (VU.map (\x -> (x - minV) / (maxV - minV) * (ub - lb) + lb)) xs
  where
    minV = L.minimum . L.map VU.minimum $ xs
    maxV = L.maximum . L.map VU.maximum $ xs
    

{-# INLINE rescaleStorableVectorList #-}

rescaleStorableVectorList :: (Double, Double) -> [VS.Vector Double] -> [VS.Vector Double]
rescaleStorableVectorList (lb, ub) xs
  | minV == maxV = xs
  | otherwise =
    L.map (VS.map (\x -> (x - minV) / (maxV - minV) * (ub - lb) + lb)) xs
  where
    minV = L.minimum . L.map VS.minimum $ xs
    maxV = L.maximum . L.map VS.maximum $ xs 
    


{-# INLINE l2norm #-}

l2norm :: VU.Vector Double -> VU.Vector Double
l2norm vec
  -- | s < 10 ** (-6) = VU.replicate (VU.length vec) 0
  | s == 0 = vec
  | otherwise = VU.map (/ s) vec
  where
    s = sqrt . VU.sum . VU.map (^ (2 :: Int)) $ vec
    
{-# INLINE l2norm' #-}

l2norm' :: VU.Vector Double -> (Double, VU.Vector Double)
l2norm' vec
  -- | s < 10 ** (-6) = VU.replicate (VU.length vec) 0
  | s == 0 = (0, vec)
  | otherwise = (s, VU.map (/ s) vec)
  where
    s = sqrt . VU.sum . VU.map (^ (2 :: Int)) $ vec
