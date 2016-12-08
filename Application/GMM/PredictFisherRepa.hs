module Main where

import           Application.GMM.ArgsParser         as Parser
import           Application.GMM.FisherKernel
import           Application.GMM.GMM
import           Application.GMM.MixtureModel
import           Classifier.LibLinear
import           Control.Arrow
import           Control.Monad
import qualified Control.Monad.Parallel             as MP
import           Control.Parallel
import           CV.Array.LabeledArray
import           CV.Feature.PolarSeparableRepa
import           CV.Filter
import           CV.Filter.PolarSeparableFilter
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

scaleConduit
  :: ParallelParams
  -> Conduit (LabeledArray DIM3 Double) IO (LabeledArray DIM3 Double)
scaleConduit parallelParams =
  do xs <- CL.take (Parallel.batchSize parallelParams)
     unless (P.null xs)
            (do let ys =
                      parMapChunk
                        parallelParams
                        rseq
                        (\(LabeledArray label arr) ->
                           LabeledArray label . computeS $ R.map (* 100) arr)
                        xs
                sourceList ys
                scaleConduit parallelParams)

main = do
  args <- getArgs
  if P.null args
    then error "run with --help to see options."
    else return ()
  params <- parseArgs args
  gmm <- readGMM (gmmFile params) :: IO [GMM]
  let parallelParams =
        ParallelParams
        { Parallel.numThread = Parser.numThread params
        , Parallel.batchSize = Parser.batchSize params
        }
      filterParamsSet1 =
        PolarSeparableFilterParamsSet
        { getSizeSet = (0, 0)
        , getDownsampleFactorSet = 1
        , getScaleSet = S.fromDistinctAscList (scale params)
        , getRadialFreqSet = S.fromDistinctAscList [0 .. (freq params - 1)]
        , getAngularFreqSet = S.fromDistinctAscList [0 .. (freq params - 1)]
        , getNameSet = Pinwheels
        }
      filterParamsSet2 =
        PolarSeparableFilterParamsSet
        { getSizeSet = (0, 0)
        , getDownsampleFactorSet = 2
        , getScaleSet = S.fromDistinctAscList (scale params)
        , getRadialFreqSet = S.fromDistinctAscList [0 .. (freq params - 1)]
        , getAngularFreqSet = S.fromDistinctAscList [0 .. (freq params - 1)]
        , getNameSet = Pinwheels
        }
      filterParamsList = [filterParamsSet1, filterParamsSet2]
      numLayer = 2
      numFeature = numLayer * (P.sum . P.map getFilterNum $ filterParamsList)
  print params
  readLabeledImagebinarySource (inputFile params) $$ scaleConduit parallelParams =$=
    labeledArrayMagnitudeSetVariedSizeConduit
      parallelParams
      filterParamsList
      (downsampleFactor params) =$=
    (fisherVectorConduit parallelParams gmm) =$=
    CL.map (fromIntegral *** (getFeature . Dense . VU.toList)) =$=
    predict (modelName params) ((modelName params) P.++ ".out")
