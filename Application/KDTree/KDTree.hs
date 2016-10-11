module Application.KDTree.KDTree where

import           CV.Feature.PolarSeparable
import           CV.Feature.PolarSeparable
import           CV.Utility.Parallel
import           Data.Vector.Unboxed       as VU
import           Prelude                   as P
import           Data.Conduit
import           Data.Conduit.List                  as CL
import           Data.KdTree.Static                     as KDT

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
    

