module CV.Feature.PolarSeparableRepa where

import           CV.Feature.PolarSeparable
import           CV.Filter
import           CV.Filter.PolarSeparableFilter
import           CV.Utility.Parallel
import           Data.Array.Repa                as R
import           Data.Complex                   as C
import           Data.Conduit
import           Data.Conduit.List              as CL

magnitudeConduit
  :: PolarSeparableFilter (R.Array U DIM3 (Complex Double))
  -> Int
  -> Conduit (R.Array U DIM3 Double) IO (R.Array U DIM3 Double)
magnitudeConduit filter factor = awaitForever (\x -> undefined)
