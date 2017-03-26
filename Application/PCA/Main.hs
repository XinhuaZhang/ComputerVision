import           CV.Statistics.PCA
import           CV.Utility.Parallel
import           Data.List             as L
import           Data.Ord              (comparing)
import           Data.Vector   as V
import           Data.Vector.Unboxed   as VU
import           Data.Vector   as V
import           Numeric.LinearAlgebra as LA
import Numeric.Statistics.PCA
import Data.Array as Arr

main = do
  let parallelParams =
        ParallelParams
        { numThread = 1
        , batchSize = 1
        }
      a = VU.fromListN 5 [1 .. 5]
      b = VU.fromListN 5 [2.1, 1.2, 3.5, 7, 10]
      c = VU.fromListN 5 [300,1,250,88,66]
      d = VU.fromListN 5 [-1.5,1,-77,2.3,66]
      xs = [a, b, c, d]
      ys = L.map VU.fromList . L.transpose . L.map VU.toList $ xs
      n = 5
      (eigenValueLib,eigenVecLib) = pcaN (listArray (0,3) . L.map (LA.fromList) . L.transpose . L.map VU.toList$ ys ) n
      (pcaMatSVD,eigenValueSVD, zs) = pcaSVD parallelParams n ys
  (pcaMat,eigenValueCov, ys) <- pcaCovariance parallelParams n ys
  -- Prelude.mapM_ print $ L.zipWith VU.zip ys zs
  -- Prelude.mapM_ print $ L.zipWith (VU.zipWith (-)) ys zs
  -- print eigenValueCov
  print . VU.map (\x' -> x'^ (2::Int) / (fromIntegral $ L.length ys - 1)) $ eigenValueSVD
  print eigenValueCov
  print eigenValueLib
  print "hehe"
  V.mapM_ print . pcaMatrix $ pcaMatSVD
  print eigenVecLib




