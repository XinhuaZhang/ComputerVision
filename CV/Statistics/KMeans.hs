{-# LANGUAGE BangPatterns  #-}

module CV.Statistics.KMeans
  ( ClusterCenter
  , KMeansModel(..)
  , kmeans
  , computeSoftAssignment
  , computeSoftAssignmentP
  ) where

import           Control.Arrow       ((&&&))
import           Control.Monad       as M
import           CV.Utility.Parallel
import           Data.Binary
import           Data.List           as L
import           Data.Vector         as V
import           Data.Vector.Unboxed as VU
import           GHC.Generics
import           System.Random

type ClusterCenter = V.Vector (VU.Vector Double)

data KMeansModel = KMeansModel
  { clusterSize :: V.Vector Int
  , center      :: ClusterCenter
  }

instance Binary KMeansModel where
  put (KMeansModel cs c) = do
    put . V.toList $ cs
    put . L.map VU.toList . V.toList $ c
  get = do
    cs <- get
    xs <- get
    return $! KMeansModel (V.fromList cs) . V.fromList . L.map VU.fromList $ xs

type Assignment = V.Vector Int

randomClusterCenterPP
  :: [VU.Vector Double]
  -> Int
  -> [V.Vector (VU.Vector Double)]
  -> IO [VU.Vector Double]
randomClusterCenterPP !centers !0 _ = return centers
randomClusterCenterPP [] !n xs = do
  randomNumber <-
    M.replicateM (L.length xs) .
    V.replicateM (V.length . L.head $ xs) . randomRIO $
    ((0, 1) :: (Double, Double))
  let ys =
        V.concat $
        parZipWith
          rdeepseq
          (\x' r' -> V.map fst . V.filter (\(_, p') -> p' > 0.5) $ V.zip x' r')
          xs
          randomNumber
  randomClusterCenterPP
    [VU.map (/ fromIntegral (V.length ys)) . V.foldl1' (VU.zipWith (+)) $ ys]
    (n - 1)
    xs
randomClusterCenterPP !centers !n xs = do
  randomNumber <-
    M.replicateM (L.length xs) .
    V.replicateM (V.length . L.head $ xs) . randomRIO $
    ((0, 1) :: (Double, Double))
  let ys =
        parMap
          rdeepseq
          (\vec' -> V.map (\x' -> L.minimum $ L.map (distFunc x') centers) vec')
          xs
      zs = parMap rdeepseq V.sum ys
      s = L.sum zs
      as =
        V.concat $
        parZipWith3
          rdeepseq
          (\xs' ys' rs' ->
             V.map fst . V.filter snd $
             V.zipWith3
               (\x' y' r' ->
                  if y' / s > r'
                    then (x', True)
                    else (x', False))
               xs'
               ys'
               rs')
          xs
          ys
          randomNumber
      center =
        VU.map (/ fromIntegral (V.length as)) . V.foldl1' (VU.zipWith (+)) $ as
  if V.length as == 0
    then randomClusterCenterPP centers n xs
    else randomClusterCenterPP (center : centers) (n - 1) xs

{-# INLINE computeAssignmentP #-}
computeAssignmentP :: ClusterCenter
                   -> [V.Vector (VU.Vector Double)]
                   -> [Assignment]
computeAssignmentP = parMap rdeepseq . computeAssignment

{-# INLINE computeMeanP #-}
computeMeanP
  :: Int
  -> Int
  -> [Assignment]
  -> [V.Vector (VU.Vector Double)]
  -> [V.Vector (VU.Vector Double)]
computeMeanP k nf = parZipWith rdeepseq (computeMean k nf)

{-# INLINE computeDistortionP #-}
computeDistortionP :: ClusterCenter
                   -> [Assignment]
                   -> [V.Vector (VU.Vector Double)]
                   -> Double
computeDistortionP clusterCenter assignments =
  L.sum . parZipWith rdeepseq (computeDistortion clusterCenter) assignments

{-# INLINE meanList2ClusterCenter #-}
meanList2ClusterCenter
  :: [V.Vector (VU.Vector Double)]
  -> [V.Vector (VU.Vector Double)]
  -> IO ClusterCenter
meanList2ClusterCenter vecs xss =
  M.liftM V.fromList . randomClusterCenterPP zs (k - L.length zs) $ xss
  where
    xs =
      L.map
        (V.map
           (\x ->
              if VU.any isNaN x
                then (undefined, 0)
                else (x, 1)))
        vecs
    ys =
      L.foldl'
        (V.zipWith
           (\(s, count) (vec, n) ->
              if n == 0
                then (s, count)
                else (VU.zipWith (+) s vec, count + 1)))
        (V.replicate k (VU.replicate nf 0, (0 :: Double))) $
      xs
    zs =
      V.toList .
      V.map (\(s, count) -> VU.map (/ count) s) .
      V.filter (\(_, count) -> count /= 0) $
      ys
    nf = VU.length . V.head . L.head $ vecs
    k = V.length . L.head $ vecs

computeClusterSize :: Int -> [Assignment] -> V.Vector Int
computeClusterSize k xs =
  V.accumulate (+) (V.replicate k 0) $ V.zip vec (V.replicate (V.length vec) 1)
  where
    vec = V.concat xs

kmeans :: ParallelParams -> Int -> [VU.Vector Double] -> IO KMeansModel
kmeans parallelParams k xs = do
  randomCenter <- randomClusterCenterPP [] k ys
  go (V.fromListN k randomCenter) (fromIntegral (maxBound :: Int)) $ ys
  where
    nf = VU.length . L.head $ xs
    ys =
      L.map V.fromList .
      splitList (div (L.length xs) (numThread parallelParams)) $
      xs
    go !center lastDistortion zs = do
      let assignment = computeAssignmentP center zs
          distortion = computeDistortionP center assignment zs
      print distortion
      if distortion >= lastDistortion
        then return $! KMeansModel (computeClusterSize k assignment) center
        else do
          newCenter <-
            meanList2ClusterCenter (computeMeanP k nf assignment zs) zs
          go newCenter distortion zs

{-# INLINE computeSoftAssignment #-}
computeSoftAssignment :: ClusterCenter -> VU.Vector Double -> VU.Vector Double
computeSoftAssignment center x =
  V.convert . V.map (\y -> max 0 (mean - y)) $ dist
  where
    dist = V.map (distFunc x) center
    mean = V.sum dist / fromIntegral (V.length center)

computeSoftAssignmentP :: ParallelParams
                       -> ClusterCenter
                       -> [VU.Vector Double]
                       -> [VU.Vector Double]
computeSoftAssignmentP parallelParams center =
  parMapChunk parallelParams rdeepseq (computeSoftAssignment center)

{-# INLINE computeAssignment #-}
computeAssignment :: ClusterCenter -> V.Vector (VU.Vector Double) -> Assignment
computeAssignment cluster =
  V.map (\x -> V.minIndex . V.map (distFunc x) $ cluster)

{-# INLINE computeMean #-}
computeMean
  :: Int
  -> Int
  -> Assignment
  -> V.Vector (VU.Vector Double)
  -> V.Vector (VU.Vector Double)
computeMean k nf assignmet =
  V.map (\(s, count) -> VU.map (/ count) s) .
  V.accumulate
    (\(s, count) vec -> (VU.zipWith (+) s vec, count + 1))
    (V.replicate k (VU.replicate nf 0, 0)) .
  V.zip assignmet

{-# INLINE computeDistortion #-}
computeDistortion :: ClusterCenter
                  -> Assignment
                  -> V.Vector (VU.Vector Double)
                  -> Double
computeDistortion clusterCenter assignments =
  V.sum .
  V.zipWith
    (\assignment vec -> distFunc vec (clusterCenter V.! assignment))
    assignments

{-# INLINE distFunc #-}
distFunc :: VU.Vector Double -> VU.Vector Double -> Double
distFunc vec1 vec2 =
  VU.sum $ VU.zipWith (\a b -> (a - b) ^ (2 :: Int)) vec1 vec2

{-# INLINE splitList #-}
splitList :: Int -> [a] -> [[a]]
splitList _ [] = []
splitList n ys = as : splitList n bs
  where
    (as, bs) = L.splitAt n ys