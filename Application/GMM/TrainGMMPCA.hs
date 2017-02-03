{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
module Main where

import           Application.GMM.ArgsParser     as Parser
import           Application.MultiDimensionalGMM.GMM
import           Application.GMM.PCA
import           Control.Monad                  as M
import           Control.Monad.Trans.Resource
import           CV.Array.LabeledArray
import           CV.Feature.PolarSeparable
import           CV.Filter.PolarSeparableFilter
import           CV.Utility.Parallel            as Parallel
import           Data.Array.Repa                as R
import           Data.Binary
import           Data.ByteString.Lazy           as BL
import           Data.Conduit
import           Data.Conduit.Binary            as CB
import           Data.Conduit.List              as CL
import           Data.List                      as L
import           Data.Set                       as S
import           Numeric.LinearAlgebra.Data     as LA
import           Prelude                        as P
import           System.Directory
import           System.Environment
import           System.IO                      as IO
import           CV.Utility.RepaArrayUtility
import           CV.Filter.GaussianFilter

downsampleConduit
  :: (R.Source s Double)
  => ParallelParams
  -> Int
  -> Conduit (R.Array s DIM3 Double) (ResourceT IO) (R.Array D DIM3 Double)
downsampleConduit parallelParams factor' =
  do xs <- CL.take (Parallel.batchSize parallelParams)
     let ys =
           parMapChunk
             parallelParams
             rseq
             (\x ->
                let y =
                      downsample [factor',factor',1]
                                 x
                in y)
             xs
     sourceList ys
     downsampleConduit parallelParams factor'

main =
  do args <- getArgs
     if P.null args
        then error "run with --help to see options."
        else return ()
     params <- parseArgs args
     print params
     imageSize <-
       if isFixedSize params
          then do xs <-
                    runResourceT $
                    sourceFile (inputFile params) $$
                    readLabeledImagebinaryConduit =$=
                    CL.take 1
                  let (LabeledArray _ arr) = L.head xs
                      (Z :. _ :. ny :. nx) = extent arr
                  return (ny,nx)
          else return (0,0)
     pcaMatrixes <- readMatrixes (pcaFile params)
     images <-
       readLabeledImageBinary (inputFile params)
                              (numGMMExample params)
     let parallelParams =
           ParallelParams {Parallel.numThread = Parser.numThread params
                          ,Parallel.batchSize = Parser.batchSize params}
         filterParamsSet1 =
           PolarSeparableFilterParamsSet {getSizeSet = imageSize
                                         ,getDownsampleFactorSet = 1
                                         ,getScaleSet =
                                            S.fromDistinctAscList (scale params)
                                         ,getRadialFreqSet =
                                            S.fromDistinctAscList
                                              [0 .. (freq params - 1)]
                                         ,getAngularFreqSet =
                                            S.fromDistinctAscList
                                              [0 .. (freq params - 1)]
                                         ,getNameSet = Pinwheels}
         filterParamsSet2 =
           PolarSeparableFilterParamsSet {getSizeSet = imageSize
                                         ,getDownsampleFactorSet = 2
                                         ,getScaleSet =
                                            S.fromDistinctAscList (scale params)
                                         ,getRadialFreqSet =
                                            S.fromDistinctAscList
                                              [0 .. (freq params - 1)]
                                         ,getAngularFreqSet =
                                            S.fromDistinctAscList
                                              [0 .. (freq params - 1)]
                                         ,getNameSet = Pinwheels}
         gaussianFilterParams =
           GaussianFilterParams (L.head $ scale params)
                                imageSize
         filterParamsSetList =
           L.take (numLayer params)
                  [filterParamsSet1,filterParamsSet2,filterParamsSet2]
         numM = numGaussian params
         magnitudeConduit filterParams =
           if isFixedSize params
              then if isComplex params
                      then singleLayerComplexFixedSizedConduit parallelParams
                                                               (makeFilterSet filterParams)
                      else singleLayerMagnitudeFixedSizedConduit parallelParams
                                                                 (makeFilterSet filterParams)
              else if isComplex params
                      then singleLayerComplexVariedSizedConduit parallelParams filterParams
                      else singleLayerMagnitudeVariedSizedConduit parallelParams filterParams
         imgArrs = L.map (\(LabeledArray _ arr) -> arr) images
     withBinaryFile (gmmFile params)
                    WriteMode $
       \h ->
         M.foldM (\arrs (filterParamsSet,pcaMat) ->
                    do filteredArrs <-
                         runResourceT $
                         sourceList arrs $$ magnitudeConduit filterParamsSet =$=
                         CL.consume
                       runResourceT $
                         CL.sourceList filteredArrs $$
                         gaussianVariedSizeConduit parallelParams gaussianFilterParams =$=
                         CL.map (downsample [(downsampleFactor params),(downsampleFactor params),1]) =$=
                         -- downsampleConduit parallelParams
                         --                   (downsampleFactor params) =$=
                         extractPointwiseFeatureConduit parallelParams =$=
                         pcaConduit parallelParams pcaMat =$=
                         hGMMSink1 parallelParams
                                   h
                                   (numGaussian params)
                                   (threshold params)
                                   (numGMMExample params)
                       return filteredArrs)
                 imgArrs $
         L.zip filterParamsSetList pcaMatrixes
