module Main where

import           Application.GMM.ArgsParser         as Parser
import           Application.GMM.FisherKernelMPPCA
import           Application.GMM.MPPCA
import           Application.GMM.MixtureModel
import           Classifier.LibLinear
import           Control.Monad.IO.Class
import qualified Control.Monad.Parallel             as MP
import           Control.Parallel
import           CV.Array.LabeledArray
import           CV.Feature.PolarSeparableRepa
import           CV.Filter
import           CV.Filter.PolarSeparableFilter
import           CV.Filter.PolarSeparableFilterRepa
import           CV.IO.ImageIO
import           CV.Utility.Parallel                as Parallel
import           Data.Array.Repa                    as R
import           Data.Binary
import           Data.Complex                       as C
import           Data.Conduit
import           Data.Conduit.List                  as CL
import           Data.List                          as L
import           Data.Maybe                         as Maybe
import           Data.Set                           as S
import           Data.Time.LocalTime
import           Data.Vector                        as V
import           Data.Vector.Unboxed                as VU
import           Foreign.Ptr
import           Prelude                            as P
import           System.Environment

main = do
  args <- getArgs
  if P.null args
    then error "run with --help to see options."
    else return ()
  params <- parseArgs args
  mppca <- decodeFile (gmmFile params) :: IO MPPCA
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
        makeFilter filterParams :: PolarSeparableFilter (R.Array U DIM3 (C.Complex Double))
  print params
  readLabeledImagebinarySource (inputFile params) $$
    magnitudeLabeledArrayConduit' parallelParams filters (downsampleFactor params) =$=
    CL.map
      (\(LabeledArray label arr) ->
          let (Z :. nf :. ny :. nx) = extent arr
              r = (fromIntegral $ nx ^ 2 + ny ^ 2) / 4
              centerX = fromIntegral nx / 2
              centerY = fromIntegral ny / 2
              vec =
                P.map
                  (\(a, b) ->
                      if (fromIntegral b - centerX) ^ 2 + (fromIntegral a - centerY) ^ 2 < r
                        then Just . toUnboxed . computeS $
                             R.slice arr (Z :. All :. a :. b)
                        else Nothing) $
                [ (i, j)
                | i <- [0 .. ny - 1]
                , j <- [0 .. nx - 1] ]
          in (label, V.fromList . Maybe.catMaybes $ vec)) =$=
    (fisherVectorConduit parallelParams  mppca) =$=
    CL.mapM (\(label, xs) ->  do ptr <- getFeatureVecPtr . Dense . VU.toList $ xs
                                 return (fromIntegral label , ptr)) =$=
    predict (modelName params) ((modelName params) P.++ ".out")
