{-# LANGUAGE FlexibleContexts #-}
import           Application.MultiDimensionalGMM.GMM
import           Application.PinwheelPCANet.ArgsParser as Parser
import           Application.PinwheelPCANet.PCA
import           Application.PinwheelPCANetMax.Pooling
import           Control.Monad                         as M
import           Control.Monad.Trans.Resource
import           CV.Array.LabeledArray
import           CV.Feature.PolarSeparable
import           CV.Filter.PolarSeparableFilter
import           CV.Utility.Parallel                   as Parallel
import           CV.Utility.RepaArrayUtility
import           Data.Array.Repa                       as R
import           Data.Binary
import           Data.ByteString.Lazy                  as BL
import           Data.Conduit
import           Data.Conduit.Binary                   as CB
import           Data.Conduit.List                     as CL
import           Data.List                             as L
import           Data.Set                              as S
import           Numeric.LinearAlgebra.Data            as LA
import           Prelude                               as P
import           System.Directory
import           System.Environment
import           System.IO                             as IO

main = do
  args <- getArgs
  if P.null args
    then error "run with --help to see options."
    else return ()
  params <- parseArgs args
  print params
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
  pcaMatrixes <- readMatrixes (pcaFile params)
  images <- readLabeledImageBinary (inputFile params) (numGMMExample params)
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
      filterParamsSetList =
        L.zipWith filterParamsSetFunc [1, 2, 2, 2, 1] (freq params)
      numM = numGaussian params
      magnitudeConduit filterParams =
        if isFixedSize params
          then singleLayerMagnitudeFixedSizedConduit
                 parallelParams
                 (makeFilterSet filterParams)
          else singleLayerMagnitudeVariedSizedConduit parallelParams filterParams
      imgArrs = L.map (\(LabeledArray _ arr) -> arr) images
  withBinaryFile (gmmFile params) WriteMode $
    \h ->
       M.foldM
         (\arrs (filterParamsSet, pcaMat, downsampleFactor') -> do
            filteredArrs <-
              runResourceT $
              sourceList arrs $$ magnitudeConduit filterParamsSet =$=
              poolConduit parallelParams Max 3 =$=
              pcaConduit parallelParams pcaMat =$=
              CL.consume
            runResourceT $
              CL.sourceList filteredArrs $$
              CL.map (downsample [downsampleFactor', downsampleFactor', 1]) =$=
              extractPointwiseFeatureConduit parallelParams =$=
              hGMMSink1
                parallelParams
                h
                (numGaussian params)
                (threshold params)
                (numGMMExample params)
            return filteredArrs)
         imgArrs $
       L.zip3 filterParamsSetList pcaMatrixes (downsampleFactor params) 
