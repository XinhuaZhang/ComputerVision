{-# LANGUAGE BangPatterns #-}

module Main where

import           Application.GMM.ArgsParser     as Parser
import           Application.GMM.FisherKernel
import           Application.GMM.GMM
import           Application.GMM.MixtureModel
import           Application.GMM.PCA
import           Classifier.LibLinear
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           Control.Parallel
import           CV.Array.LabeledArray
import           CV.Feature.PolarSeparableRepa
import           CV.Filter.PolarSeparableFilter
import           CV.Utility.Parallel            as Parallel
import           CV.Utility.Time
import           Data.Array.Repa                as R
import           Data.Conduit
import           Data.Conduit.Binary            as CB
import           Data.Conduit.List              as CL
import           Data.List                      as L
import           Data.Set                       as S
import           Data.Vector.Unboxed            as VU
import           Foreign.Ptr
import           Prelude                        as P
import           System.Environment

trainSink
  :: ParallelParams
  -> FilePath
  -> TrainParams
  -> Bool
  -> Sink (Int, VU.Vector Double) (ResourceT IO) ()
trainSink parallelParams filePath trainParams findCFlag = go [] []
  where
    go :: [[Double]]
       -> [[Ptr C'feature_node]]
       -> Sink (Int, VU.Vector Double) (ResourceT IO) ()
    go label pss = do
      xs <- CL.take (Parallel.batchSize parallelParams)
      if P.length xs > 0
        then do
          let (ls, ys) = P.unzip xs
          ps <- liftIO $ P.mapM (getFeatureVecPtr . Dense . VU.toList) ys
          liftIO $ printCurrentTime
          go ((P.map fromIntegral ls) : label) $! (ps : pss)
        else liftIO $
             train
               trainParams
               (P.concat . L.reverse $ label)
               (P.concat . L.reverse $ pss)

main = do
  args <- getArgs
  if P.null args
    then error "run with --help to see options."
    else return ()
  params <- parseArgs args
  gmm <- readGMM (gmmFile params) :: IO [GMM]
  pcaMatrix <- readMatrix (pcaFile params)
  imageListLen <- getArrayNumFile (inputFile params)
  imageSize <-
    if isFixedSize params
      then do
        xs <-
          runResourceT $
          sourceFile (inputFile params) $$ readLabeledImagebinaryConduit =$=
          CL.take 1
        let (LabeledArray _ arr) = L.head xs
            (Z :. _ :. ny :. nx) = extent arr
        return (ny, nx)
      else return (0, 0)
  let parallelParams =
        ParallelParams
        { Parallel.numThread = Parser.numThread params
        , Parallel.batchSize = Parser.batchSize params
        }
      filterParamsSet1 =
        PolarSeparableFilterParamsSet
        { getSizeSet = imageSize
        , getDownsampleFactorSet = 1
        , getScaleSet = S.fromDistinctAscList (scale params)
        , getRadialFreqSet = S.fromDistinctAscList [0 .. (freq params - 1)]
        , getAngularFreqSet = S.fromDistinctAscList [0 .. (freq params - 1)]
        , getNameSet = Pinwheels
        }
      filterParamsSet2 =
        PolarSeparableFilterParamsSet
        { getSizeSet = imageSize
        , getDownsampleFactorSet = 2
        , getScaleSet = S.fromDistinctAscList (scale params)
        , getRadialFreqSet = S.fromDistinctAscList [0 .. (freq params - 1)]
        , getAngularFreqSet = S.fromDistinctAscList [0 .. (freq params - 1)]
        , getNameSet = Pinwheels
        }
      filterParamsList = [filterParamsSet1, filterParamsSet2]
      numFeature =
        L.sum . L.map L.product . L.tail . L.inits . L.map getFilterNum $ filterParamsList
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
      magnitudeConduit =
        if isFixedSize params
          then labeledArrayMagnitudeSetFixedSizeConduit
                 parallelParams
                 (L.map makeFilterSet filterParamsList)
                 (downsampleFactor params)
          else labeledArrayMagnitudeSetVariedSizeConduit
                 parallelParams
                 filterParamsList
                 (downsampleFactor params)
  print params
  runResourceT $
    sourceFile (inputFile params) $$ readLabeledImagebinaryConduit =$= magnitudeConduit =$=
    pcaLabelConduit parallelParams pcaMatrix =$=
    (fisherVectorConduit parallelParams gmm) =$=
    trainSink parallelParams (labelFile params) trainParams (findC params)
