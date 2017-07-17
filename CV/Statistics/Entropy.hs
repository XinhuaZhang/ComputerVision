module CV.Statistics.Entropy
  ( module CV.Statistics.Histogram
  , module CV.Statistics.Entropy
  ) where

import           Control.DeepSeq
import           CV.Statistics.Histogram
import           CV.Utility.Parallel
import           Data.List               as L

entropy
  :: (Num a, Floating a)
  => KdHist a -> a
entropy (KdHist _ bins) = -1 * (L.sum . L.map ((\x -> x * log x) . (/ s)) $ xs)
  where
    xs = L.map (\(Bin _ n) -> n) bins
    s = L.sum xs
    
-- conditionalEntropy hist idx: H(All - idx | idx)
conditionalEntropy
  :: (Num a, Floating a)
  => KdHist a -> [Int] -> a
conditionalEntropy hist idx =
  entropy hist - (entropy . computeMarginalHistogram hist $ idx)

-- mutualInformation hist idx :: I(All - idx , idx)
mutualInformation
  :: (Num a, Floating a)
  => KdHist a -> [Int] -> a
mutualInformation hist idx =
  (entropy . computeMarginalHistogram hist $ idx) +
  (entropy . computeMarginalHistogram hist $ idx') -
  entropy hist
  where
    idx' = [0 .. getNumDims hist - 1] \\ idx

computeSVMKernel
  :: (Ord a, Num a, NFData a)
  => ParallelParams -> [KdHist a] -> [[a]]
computeSVMKernel parallelParams xs =
  parMapChunk parallelParams rdeepseq (\x -> L.map (intersection x) xs) xs
