import           CV.Statistics.PCA
import           CV.Utility.Parallel
import           Data.List             as L
import           Data.Ord              (comparing)
import           Data.Vector   as V
import           Data.Vector.Unboxed   as VU
import           Numeric.LinearAlgebra as LA

main = do
  let parallelParams =
        ParallelParams
        { numThread = 1
        , batchSize = 1
        }
      a = VU.fromListN 5 [1 .. 5]
      b = VU.fromListN 5 [2.1, 1.2, 3.5, 7, 10]
      c = VU.fromListN 5 [300,1,250,88,66]
      xs = [a, b, c]
      (mean, ys) = computeRemoveMean parallelParams xs
      d'' = fromRows . L.map (LA.fromList . VU.toList) $ ys
      (_, vec', uni') = thinSVD d''
      vec = LA.toList vec'
      uni =
        L.map (VU.fromList . LA.toList) . toColumns $ uni' :: [VU.Vector Double]
      v' = L.zip vec uni
      v = L.take 2 . snd . L.unzip . L.reverse $ sortBy (comparing fst) v'
      -- pcaMatSVD = PCAMatrix mean (V.fromList v)
      -- zs = pcaReduction parallelParams pcaMatSVD xs
      n = 4
      (pcaMatSVD, zs) = pcaSVD parallelParams n xs
  (pcaMat, ys) <- pcaCovariance parallelParams n xs
  print . size $ uni'
  Prelude.mapM_ print $ L.zipWith VU.zip ys zs
  Prelude.mapM_ print $ L.zipWith (VU.zipWith (-)) ys zs

