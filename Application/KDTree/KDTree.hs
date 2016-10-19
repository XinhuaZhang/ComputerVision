{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE TypeFamilies      #-}

module Application.KDTree.KDTree
  ( buildTreeConduit
  , similarity
  , pointAsList
  , dist
  , writeKdTree
  , readKdTree
  ) where


import           Application.KDTree.KdMapStatic  as KDM
import           Application.KDTree.KdTreeStatic as KDT
import           Control.DeepSeq
import           Control.Monad.IO.Class
import           Control.Parallel
import           CV.Feature.PolarSeparable
import           CV.Utility.Parallel
import           Data.Array                      as IA
import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put
import           Data.Conduit
import           Data.Conduit.List               as CL
import           Prelude                         as P

pointAsList :: PolarSeparableFeaturePoint -> [Double]
pointAsList = feature

buildTreeConduit
  :: ParallelParams
  -> Conduit [PolarSeparableFeaturePoint] IO (KdTree Double PolarSeparableFeaturePoint)
buildTreeConduit parallelParams = do
  xs <- CL.take (batchSize parallelParams)
  if P.length xs > 0
    then do
      sourceList $ parMapChunk parallelParams rdeepseq (KDT.build pointAsList) xs
      buildTreeConduit parallelParams
    else return ()

similarity
  :: (KdTree Double PolarSeparableFeaturePoint, Array (Int,Int) PolarSeparableFeaturePoint)
  -> (KdTree Double PolarSeparableFeaturePoint, Array (Int,Int) PolarSeparableFeaturePoint)
  -> Double
  -> Int
  -> Double
similarity (treeX, arrX) (treeY, arrY) radius sampleRate
  | P.or . P.map ((== 0) . KDT.size) $ [treeX, treeY] = error "KdTree is empty."
  | P.or . P.map (<sampleRate) $ [nyX,nxX,nyY,nxY] = error "The kdTree is smaller than the sampling grid."
  | otherwise = par hehe (pseq haha (2 / (hehe + haha + 0.001)))  -- KL-divergence is always non-negative
  where
    hehe = (klDivergence xx yx)
    haha = (klDivergence yy xy)
    (_, (nyX, nxX)) = bounds arrX
    intervalxX = div nxX sampleRate
    intervalyX = div nyX sampleRate
    xs =
      [ arrX IA.! (i * intervalyX, j * intervalxX)
      | i <- [1 .. sampleRate - 1]
      , j <- [1 .. sampleRate - 1] ]
    (_, (nyY, nxY)) = bounds arrY
    intervalxY = div nxY sampleRate
    intervalyY = div nyY sampleRate
    ys =
      [ arrY IA.! (i * intervalyY, j * intervalxY)
      | i <- [1 .. sampleRate - 1]
      , j <- [1 .. sampleRate - 1] ]
    xx = probabilityP treeX xs radius
    xy = probabilityQ treeX ys radius
    yx = probabilityQ treeY xs radius
    yy = probabilityP treeY ys radius

probabilityP
  :: KdTree Double PolarSeparableFeaturePoint
  -> [PolarSeparableFeaturePoint]
  -> Double
  -> [Double]
probabilityP tree xs radius
  | s == (P.length xs) * (KDT.size tree) =
    error "Radius is too large. Found every point."
  | otherwise = P.map (\x -> (fromIntegral x) / (fromIntegral s)) num
  where
    num = P.map (KDT.inRadiusCount tree radius) xs
    s = P.sum $!! num

probabilityQ
  :: KdTree Double PolarSeparableFeaturePoint
  -> [PolarSeparableFeaturePoint]
  -> Double
  -> [Double]
probabilityQ tree xs radius
  | s == 0 = cycle [0]
  | otherwise = P.map (\x -> (fromIntegral x) / (fromIntegral s)) num
  where
    num = P.map (KDT.inRadiusCount tree radius) xs
    s = P.sum $!! num

klDivergence :: [Double] -> [Double] -> Double
klDivergence xs ys =
  P.sum .
  P.map (\(x, y) -> x * log (x / ((x + y) * 0.5))) .
  P.filter (\(x, y) -> x /= 0) $
  P.zip xs ys

dist :: PolarSeparableFeaturePoint -> PolarSeparableFeaturePoint -> Double
dist xs ys =
  (P.sum $ P.map (^ 2) $ P.zipWith (-) (feature xs) (feature ys))
  -- /
  -- (P.fromIntegral $ VU.length . feature $ xs)^2


writeKdTree :: FilePath -> [KdTree Double PolarSeparableFeaturePoint] -> IO ()
writeKdTree filePath =
  encodeFile filePath . P.map (\(KdTree (KdMap _ _ node size)) -> (node, size))


readKdTree :: FilePath -> IO [KdTree Double PolarSeparableFeaturePoint]
readKdTree filePath = do
  pairs <- decodeFile filePath
  return $
    P.map
      (\(node, size) ->
          KdTree (KdMap pointAsList (KDT.defaultSqrDist pointAsList) node size))
      pairs
