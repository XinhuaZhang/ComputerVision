{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE TypeFamilies      #-}
module Application.KDTree.KDTree
  ( buildTreeConduit
  , similarity
  ) where

import           CV.Feature.PolarSeparable
import           CV.Utility.Parallel
import           Data.Conduit
import           Data.Conduit.List         as CL
import           Data.KdTree.Static        as KDT
import           Data.Vector.Unboxed       as VU
import           Prelude                   as P

pointAsList
  :: PolarSeparableFeaturePoint -> [Double]
pointAsList = VU.toList . feature

buildTreeConduit
  :: ParallelParams
  -> Conduit [PolarSeparableFeaturePoint] IO (KdTree Double PolarSeparableFeaturePoint)
buildTreeConduit parallelParams = do
  xs <- CL.take (batchSize parallelParams)
  if P.length xs > 0
    then do
      sourceList $ parMapChunk parallelParams rdeepseq (build pointAsList) xs
      buildTreeConduit parallelParams
    else return ()


similarity
  :: KdTree Double PolarSeparableFeaturePoint
  -> KdTree Double PolarSeparableFeaturePoint
  -> Double
  -> Double
similarity treeX treeY radius
  | P.or . P.map ((== 0) . size) $ [treeX, treeY] = error "KdTree is empty."
  | otherwise = ((klDivergence xx yx) + (klDivergence xy yy)) / 2
  where
    xs = KDT.toList treeX
    ys = KDT.toList treeY
    xx = probability treeX xs radius
    xy = probability treeX ys radius
    yx = probability treeY xs radius
    yy = probability treeY ys radius

probability
  :: KdTree Double PolarSeparableFeaturePoint
  -> [PolarSeparableFeaturePoint]
  -> Double
  -> [Double]
probability tree xs radius =
  P.map
    (\x ->
        fromIntegral (P.length . inRadius tree radius $ x) /
        fromIntegral (size tree))
    xs

klDivergence :: [Double] -> [Double] -> Double
klDivergence xs ys =
  P.sum .
  P.map (\(x, y) -> x * log (x / y)) . P.filter (\(x, y) -> x /= 0 && y /= 0) $
  P.zip xs ys
