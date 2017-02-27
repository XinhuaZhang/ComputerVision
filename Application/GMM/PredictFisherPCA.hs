module Main where

import           Application.GMM.ArgsParser                   as Parser
import           Application.GMM.PCA
import           Application.MultiDimensionalGMM.FisherKernel
import           Application.MultiDimensionalGMM.GMM
import           Application.MultiDimensionalGMM.MixtureModel
import           Classifier.LibLinear
import           Control.Arrow
import           Control.Monad
import           Control.Monad.Trans.Resource
import           CV.Array.LabeledArray
import           CV.Feature.PolarSeparable
import           CV.Filter.GaussianFilter
import           CV.Filter.PolarSeparableFilter
import           CV.Utility.Parallel                          as Parallel
import           Data.Array.Repa                              as R
import           Data.Conduit
import           Data.Conduit.Binary                          as CB
import           Data.Conduit.List                            as CL
import           Data.List                                    as L
import           Data.Set                                     as S
import           Data.Vector.Unboxed                          as VU
import           Prelude                                      as P
import           System.Environment

main = do
  args <- getArgs
  if P.null args
    then error "run with --help to see options."
    else return ()
  params <- parseArgs args
  gmm <- readGMM (gmmFile params) :: IO [GMM]
  pcaMatrixes <- readMatrixes (pcaFile params)
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
        , getAngularFreqSet = S.fromDistinctAscList [1 .. (freq params - 0)]
        , getNameSet = Pinwheels
        }
      filterParamsSet2 =
        PolarSeparableFilterParamsSet
        { getSizeSet = imageSize
        , getDownsampleFactorSet = 2
        , getScaleSet = S.fromDistinctAscList (scale params)
        , getRadialFreqSet = S.fromDistinctAscList [0 .. (div (freq params) 2 - 1)]
        , getAngularFreqSet = S.fromDistinctAscList [1 .. (div (freq params) 2 - 0)]
        , getNameSet = Pinwheels
        }
      filterParamsList = L.take (numLayer params) [filterParamsSet1, filterParamsSet2,filterParamsSet2]
      gaussianFilterParamsList =
        L.map
          (\gScale -> GaussianFilterParams gScale imageSize)
          (gaussianScale params)
      magnitudeConduit =
        if isFixedSize params
          then if isComplex params
                  then multiLayerComplexFixedSizedConduit
                         parallelParams
                         (L.map makeFilterSet filterParamsList)
                         (downsampleFactor params)
                  else multiLayerMagnitudeFixedSizedConduit
                         parallelParams
                         (L.map makeFilterSet filterParamsList)
                         gaussianFilterParamsList
                         (downsampleFactor params)
          else if isComplex params
                  then multiLayerComplexVariedSizedConduit
                         parallelParams
                         filterParamsList
                         (downsampleFactor params)
                  else multiLayerMagnitudeVariedSizedConduit
                         parallelParams
                         filterParamsList
                         gaussianFilterParamsList
                         (downsampleFactor params)
  print params
  runResourceT $
    sourceFile (inputFile params) $$ readLabeledImagebinaryConduit =$= magnitudeConduit =$=
    pcaLabelMultiLayerConduit parallelParams pcaMatrixes =$=
    (fisherVectorConduit1 parallelParams gmm) =$=
    CL.map (fromIntegral *** (getFeature . Dense . VU.toList)) =$=
    predict (modelName params) ((modelName params) P.++ ".out")
