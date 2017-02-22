{-# LANGUAGE BangPatterns #-}

module Main where

import           Application.MultiDimensionalGMM.FisherKernel
import           Application.MultiDimensionalGMM.GMM
import           Application.MultiDimensionalGMM.MixtureModel
import           Application.Pinwheel.ArgsParser        as Parser
import           Application.PinwheelPCANet.PCA
import           Classifier.LibLinear
import Control.Arrow
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           Control.Parallel
import           CV.Array.LabeledArray
import           CV.Feature.Coefficient
import           CV.Feature.PolarSeparable
import           CV.Filter.PolarSeparableFilter
import           CV.Utility.Parallel                          as Parallel
import           CV.Utility.Time
import           Data.Array.Repa                              as R
import           Data.Conduit
import           Data.Conduit.Binary                          as CB
import           Data.Conduit.List                            as CL
import           Data.List                                    as L
import           Data.Set                                     as S
import           Data.Vector.Unboxed                          as VU
import           Foreign.Ptr
import           Numeric.LinearAlgebra.Data                   as LA
import           Prelude                                      as P
import           System.Environment
import CV.Utility.RepaArrayUtility

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
  (gmm:_) <- readGMM (gmmFile params) :: IO [GMM]
  (pcaMatrixes:_) <- readMatrixes (pcaFile params)
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
      filterParamsSetFunc fa freq' =
        PolarSeparableFilterParamsSet
        { getSizeSet = imageSize
        , getDownsampleFactorSet = fa
        , getScaleSet = S.fromDistinctAscList (scale params)
        , getRadialFreqSet = S.fromDistinctAscList [0 .. (freq' - 1)]
        , getAngularFreqSet = S.fromDistinctAscList [1 .. (freq' - 0)]
        , getNameSet = Pinwheels
        }
      filterParamsList = L.zipWith filterParamsSetFunc [1] (freq params)
      numFeature = cols $ pcaMatrixes
      trainParams =
        TrainParams
        { trainSolver = L2R_L2LOSS_SVC_DUAL
        , trainC = c params
        , trainNumExamples = imageListLen
        , trainFeatureIndexMax =
          if isComplex params
            then (4 * numFeature) * (numModel  gmm)
            else (2 * numFeature) * (numModel  gmm)
        , trainModel = modelName params
        }
      magnitudeConduit filterParams =
        if isFixedSize params
          then let (PolarSeparableFilter _ filter') = makeFilterSet filterParams
                   (PolarSeparableFilter _ flippedFilter') =
                     makeFlippedFilterSet filterParams
               in labelCoefficientMagnitudeConduit
                    parallelParams
                    (learningRate params)
                    filter'
                    flippedFilter'
                    False
          else undefined
  print params
  runResourceT $
    sourceFile (inputFile params) $$ readLabeledImagebinaryConduit =$=
    magnitudeConduit (L.head filterParamsList) =$=
    pcaLabelConduit parallelParams pcaMatrixes =$=
    CL.map (second $ extractPointwiseFeature) =$=
    (fisherVectorConduit parallelParams gmm) =$=
    trainSink parallelParams (labelFile params) trainParams (findC params)
