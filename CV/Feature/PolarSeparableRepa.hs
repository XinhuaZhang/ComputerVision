{-# LANGUAGE BangPatterns #-}
module CV.Feature.PolarSeparableRepa where

import           Control.Monad.IO.Class             (liftIO)
import           CV.Filter
import           CV.Filter.PolarSeparableFilter
import           CV.Filter.PolarSeparableFilterRepa
import           CV.Utility.Parallel
import           CV.Utility.RepaArrayUtility        as RU
import           Data.Array.Repa                    as R
import           Data.Complex                       as C
import           Data.Conduit
import            Data.Conduit.List as CL
import           Data.Set                           as S
import           Prelude                            as P
import Control.Monad as M

magnitudeConduit
  :: ParallelParams -> PolarSeparableFilter (R.Array U DIM3 (Complex Double))
  -> Int
  -> Conduit (R.Array U DIM3 Double) IO (R.Array U DIM3 Double)
magnitudeConduit parallelParams filter' factor = do
  xs <- CL.take (batchSize parallelParams)
  unless
    (P.null xs)
    (do let ys =
              parMapChunk
                parallelParams
                rseq
                (\x' ->
                    let (Z :. nf :. ny :. nx) = extent x'
                        (nx':ny':_) = P.map (`div` factor) [nx, ny]
                        nxNew =
                          nx' -
                          div ((P.round . P.head . S.toDescList $ scale) * 4) factor
                        nyNew =
                          ny' -
                          div ((P.round . P.head . S.toDescList $ scale) * 4) factor
                        y' = applyFilter filter' $ x'
                        z =
                          if factor == 1
                            then y'
                            else RU.downsample [factor, factor, 1] y'
                        !result =
                          computeUnboxedS .
                          R.map C.magnitude .
                          crop
                            [div (nx' - nxNew) 2, div (ny' - nyNew) 2, 0]
                            [nxNew, nyNew, newNF] $
                          z
                    in result)
                xs
        sourceList ys
        magnitudeConduit parallelParams filter' factor)
  where
    scale = getScale . getParams $ filter'
    newNF = getFilterNum . getParams $ filter'


complexConduit
  :: ParallelParams -> PolarSeparableFilter (R.Array U DIM3 (Complex Double))
  -> Int
  -> Conduit (R.Array U DIM3 Double) IO (R.Array U DIM3 (Complex Double))
complexConduit parallelParams filter' factor = do
  xs <- CL.take (batchSize parallelParams)
  unless
    (P.null xs)
    (do let ys =
              parMapChunk
                parallelParams
                rseq
                (\x' ->
                    let (Z :. nf :. ny :. nx) = extent x'
                        (nx':ny':_) = P.map (`div` factor) [nx, ny]
                        nxNew =
                          nx' -
                          div ((P.round . P.head . S.toDescList $ scale) * 4) factor
                        nyNew =
                          ny' -
                          div ((P.round . P.head . S.toDescList $ scale) * 4) factor
                        y' = applyFilter filter' $ x'
                        z =
                          if factor == 1
                            then y'
                            else RU.downsample [factor, factor, 1] y'
                        !result =
                          computeUnboxedS .
                          crop
                            [div (nx' - nxNew) 2, div (ny' - nyNew) 2, 0]
                            [nxNew, nyNew, nf] $
                          z
                    in result)
                xs
        sourceList ys
        complexConduit parallelParams filter' factor)
  where
    scale = getScale . getParams $ filter'
