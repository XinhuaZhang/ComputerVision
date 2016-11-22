module Main where

import           Application.GMM.ArgsParser         as Parser
import           Application.GMM.GMM
import           CV.Array.LabeledArray
import           CV.Feature.PolarSeparable
import           CV.Feature.PolarSeparableRepa
import           CV.Filter
import           CV.Filter.PolarSeparableFilter
import           CV.Filter.PolarSeparableFilterRepa
import           CV.IO.ImageIO
import           CV.Utility.Parallel                as Parallel
import           Data.Array.Repa                    as R
import           Data.Conduit
import           Data.Conduit.List                  as CL
import           Data.Set                           as S
import           Data.Vector                        as V
import           Data.Vector.Unboxed                as VU
import           Prelude                            as P
import           System.Environment
import Data.Complex

main = do
  args <- getArgs
  if P.null args
    then error "run with --help to see options."
    else return ()
  params <- parseArgs args
  let parallelParams =
        ParallelParams
        { Parallel.numThread = Parser.numThread params
        , Parallel.batchSize = Parser.batchSize params
        }
      filterParams =
        PolarSeparableFilterParams
        { getRadius = 128
        , getScale = S.fromDistinctAscList (scale params)
        , getRadialFreq = S.fromDistinctAscList [0 .. (freq params - 1)]
        , getAngularFreq = S.fromDistinctAscList [0 .. (freq params - 1)]
        , getName = Pinwheels
        }
      filters =
        makeFilter filterParams :: PolarSeparableFilter (R.Array U DIM3 (Complex Double))
  print params
  readLabeledImagebinarySource (inputFile params) $$
    CL.map (\(LabeledArray _ arr) -> arr) =$=
    magnitudeConduit filters (downsampleFactor params) =$=
    CL.map
      (\arr ->
          let (Z :. nf :. ny :. nx) = extent arr
          in V.fromList .
             P.map
               (\(a, b) ->
                   toUnboxed . computeS $ R.slice arr (Z :. All :. a :. b)) $
             [ (i, j)
             | i <- [0 .. ny - 1]
             , j <- [0 .. nx - 1] ]) =$=
    gmmSink
      parallelParams
      (numGaussian params)
      (threshold params)
      (gmmFile params)
