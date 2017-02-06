{-# LANGUAGE FlexibleContexts #-}
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
import           Prelude                        as P
import           System.Directory
import           System.Environment
import           System.IO                      as IO
import           CV.Filter.GaussianFilter

main =
  do args <- getArgs
     when (P.null args) $ error "run with --help to see options."
     params <- parseArgs args
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
                                              [1 .. (freq params - 0)]
                                         ,getNameSet = Pinwheels}
         filterParamsSet2 =
           PolarSeparableFilterParamsSet {getSizeSet = imageSize
                                         ,getDownsampleFactorSet = 2
                                         ,getScaleSet =
                                            S.fromDistinctAscList (scale params)
                                         ,getRadialFreqSet =
                                            S.fromDistinctAscList
                                              [0 .. (div (freq params) 2 - 1)]
                                         ,getAngularFreqSet =
                                            S.fromDistinctAscList
                                              [1 .. (div (freq params) 2 - 0)]
                                         ,getNameSet = Pinwheels}
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
         gaussianFilterParamsList =
           L.map (\gScale -> GaussianFilterParams gScale imageSize)
                 (gaussianScale params)
         imgArrs = L.map (\(LabeledArray _ arr) -> arr) images
     print params
     withBinaryFile (pcaFile params)
                    WriteMode $
       \h ->
         M.foldM (\arrs (filterParamsSet,gaussianFilterParams,np) ->
                    do runResourceT $
                         sourceList arrs $$ magnitudeConduit filterParamsSet =$=
                         gaussianVariedSizeConduit parallelParams gaussianFilterParams =$=
                         hPCASink h
                                  (numGMMExample params)
                                  np
                                  (downsampleFactor params))
                 imgArrs $
         L.zip3 filterParamsSetList gaussianFilterParamsList (numPrincipal params)
