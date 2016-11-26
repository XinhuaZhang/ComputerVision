module Main where

import           Application.GMM.ArgsParser         as Parser
import           Application.GMM.GMM
import           CV.Array.LabeledArray
import CV.Array.Image
import           CV.Feature.PolarSeparable
import           CV.Feature.PolarSeparableRepa
import           CV.Filter
import           CV.Filter.PolarSeparableFilter
import           CV.Filter.PolarSeparableFilterRepa
import           CV.IO.ImageIO
import           CV.Utility.Parallel                as Parallel
import           Data.Array.Repa                    as R
import           Data.Complex
import           Data.Conduit
import           Data.Conduit.List                  as CL
import           Data.Set                           as S
import           Data.Vector                        as V
import           Data.Vector.Unboxed                as VU
import           Prelude                            as P
import           System.Environment

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
  xs <-
    readLabeledImagebinarySource (inputFile params) $$
    CL.map (\(LabeledArray _ arr) -> arr) =$=
    magnitudeConduit' parallelParams filters (downsampleFactor params) =$=
    CL.take 1
  let x = P.head xs
      (Z :. nf :. ny :. nx) = extent x
  P.mapM_
    (\i ->
        plotImage
          (show i P.++ ".png")
          (computeS $
           R.extend (Z :. (1 :: Int) :. All :. All) $
           R.slice x (Z :. i :. All :. All)))
    [0 .. nf - 1]
