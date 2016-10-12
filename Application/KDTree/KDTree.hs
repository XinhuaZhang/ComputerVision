{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE TypeFamilies      #-}

module Application.KDTree.KDTree
  ( buildTreeConduit
  , similarity
  , pointAsList
  ) where

import           Control.DeepSeq
import           Control.Monad.IO.Class
import           Control.Parallel
import           CV.Feature.PolarSeparable
import           CV.Utility.Parallel
import           Data.Conduit
import           Data.Conduit.List         as CL
import           Data.KdTree.Static        as KDT
import           Data.Vector.Unboxed       as VU
import           Prelude                   as P

pointAsList :: PolarSeparableFeaturePoint -> [Double]
pointAsList = VU.toList . feature

buildTreeConduit
  :: ParallelParams
  -> Conduit [PolarSeparableFeaturePoint] IO (KdTree Double PolarSeparableFeaturePoint)
buildTreeConduit parallelParams = do
  xs <- CL.take (batchSize parallelParams)
  if P.length xs > 0
    then do
      sourceList $ parMapChunk parallelParams rpar (build pointAsList) xs
      buildTreeConduit parallelParams
    else return ()

similarity
  :: KdTree Double PolarSeparableFeaturePoint
  -> KdTree Double PolarSeparableFeaturePoint
  -> Double
  -> Double
similarity treeX treeY radius
  | P.or . P.map ((== 0) . size) $ [treeX, treeY] = error "KdTree is empty."
  | otherwise =  ((klDivergence xx yx) + (klDivergence yy xy)) / (-2) -- KL-divergence is always non-negative
  where
    xs = KDT.toList treeX
    ys = KDT.toList treeY
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
    num = P.map (P.length . inRadius tree radius) xs
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
    num = P.map (P.length . inRadius tree radius) xs
    s = P.sum $!! num

klDivergence :: [Double] -> [Double] -> Double
klDivergence xs ys =
  P.sum .
  P.map (\(x, y) -> x * log (x / ((x + y) * 0.5))) .
  P.filter (\(x, y) -> x /= 0) $
  P.zip xs ys
