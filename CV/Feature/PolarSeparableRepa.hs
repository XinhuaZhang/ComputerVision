module CV.Feature.PolarSeparableRepa where

import           Control.Monad.IO.Class             (liftIO)
import           CV.Feature.PolarSeparable
import           CV.Filter
import           CV.Filter.PolarSeparableFilter
import           CV.Filter.PolarSeparableFilterRepa
import           CV.Utility.Parallel
import           CV.Utility.RepaArrayUtility        as RU
import           Data.Array.Repa                    as R
import           Data.Complex                       as C
import           Data.Conduit
import           Data.Conduit.List                  as CL
import           Data.Set                           as S
import           Prelude                            as P

magnitudeConduit
  :: PolarSeparableFilter (R.Array U DIM3 (Complex Double))
  -> Int
  -> Conduit (R.Array U DIM3 Double) IO (R.Array U DIM3 Double)
magnitudeConduit filter factor =
  awaitForever
    (\x ->
        let (Z :. nf :. ny :. nx) = extent x
            (nx':ny':_) = P.map (\a -> div a factor) [nx, ny]
            nxNew =
              nx' - (div (P.round $ (P.head $ S.toDescList scale) * 4) factor)
            nyNew =
              ny' - (div (P.round $ (P.head $ S.toDescList scale) * 4) factor)
        in do y <- liftIO . applyFilter filter $ x
              let z =
                    if factor == 1
                      then y
                      else RU.downsample [factor, factor, 1] y
              result <-
                liftIO .
                computeP .
                R.map C.magnitude .
                crop
                  [div (nx' - nxNew) 2, div (ny' - nyNew) 2, 0]
                  [nxNew, nyNew, nf] $
                z
              yield result)
  where
    scale = getScale . getParams $ filter
