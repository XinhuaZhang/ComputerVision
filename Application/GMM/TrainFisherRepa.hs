{-# LANGUAGE BangPatterns #-}
module Main where

import           Application.GMM.ArgsParser         as Parser
import           Application.GMM.FisherKernel
import           Application.GMM.GMM
import           Application.GMM.MixtureModel
import           Classifier.LibLinear
import           Control.Monad
import           Control.Monad.IO.Class
import qualified Control.Monad.Parallel             as MP
import           Control.Parallel
import           CV.Array.LabeledArray
import           CV.Feature.PolarSeparableRepa
import           CV.Filter
import           CV.Filter.PolarSeparableFilter
import           CV.IO.ImageIO
import           CV.Utility.Parallel                as Parallel
import           CV.Utility.Time
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

trainSink
  :: ParallelParams -> FilePath -> TrainParams -> Bool -> Sink (Int,VU.Vector Double) IO ()
trainSink parallelParams filePath trainParams findCFlag =
  do -- ys <- consume
     -- let (label,xs) = P.unzip ys
     -- if ((VU.length . P.head $ xs) /= (trainFeatureIndexMax trainParams))
     --    then error $
     --         "Number of feature in trainParams is not correct. (" P.++
     --         (show . VU.length . P.head $ xs) P.++
     --         " vs " P.++
     --         (show $ trainFeatureIndexMax trainParams) P.++
     --         ")"
     --    else return ()
     -- featurePtrs <- liftIO $ MP.mapM (getFeatureVecPtr . Dense . VU.toList) xs
     -- if findCFlag
     --    then liftIO $
     --         findParameterC trainParams
     --                        (P.map fromIntegral label)
     --                        featurePtrs
     --    else liftIO $ train trainParams (P.map fromIntegral label) featurePtrs
     go [] []
  where go :: [[Double]]
           -> [[Ptr C'feature_node]]
           -> Sink (Int,VU.Vector Double) IO ()
        go label pss =
          do xs <- CL.take (Parallel.batchSize parallelParams)
             if P.length xs > 0
                then do let (ls,ys) = P.unzip xs
                        ps <-
                          liftIO $
                          P.mapM (getFeatureVecPtr . Dense . VU.toList) ys
                        liftIO $ printCurrentTime
                        go ((P.map fromIntegral ls) : label) $! (ps : pss)
                else liftIO $
                     train trainParams
                           (P.concat . L.reverse $ label)
                           (P.concat . L.reverse $ pss)

main = do
  args <- getArgs
  if P.null args
    then error "run with --help to see options."
    else return ()
  params <- parseArgs args
  gmm <- readGMM (gmmFile params) :: IO [GMM]
  imageListLen <- getArrayNumFile (inputFile params)
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
      numFeature = L.sum . L.map  L.product . L.tail . L.inits . L.map getFilterNum $ filterParamsList
      trainParams =
        TrainParams
        { trainSolver = L2R_L2LOSS_SVC_DUAL
        , trainC = c params
        , trainNumExamples = imageListLen
        , trainFeatureIndexMax =
          if isComplex params
            then (4 * numFeature) * (numModel $ P.head gmm)
            else (2 * numFeature) * (numModel $ P.head gmm)
        , trainModel = modelName params
        }
  print params
  readLabeledImagebinarySource (inputFile params) $$ -- scaleConduit parallelParams =$=
    labeledArrayMagnitudeSetVariedSizeConduit
      parallelParams
      filterParamsList
      (downsampleFactor params) =$=
    (fisherVectorConduit parallelParams gmm) =$=
    trainSink parallelParams (labelFile params) trainParams (findC params)
