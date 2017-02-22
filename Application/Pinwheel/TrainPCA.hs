{-# LANGUAGE FlexibleContexts #-}
import           Application.MultiDimensionalGMM.GMM
import           Application.Pinwheel.ArgsParser as Parser
import           Application.PinwheelPCANet.PCA
import           Control.Monad                         as M
import           Control.Monad.Trans.Resource
import           CV.Array.LabeledArray
import           CV.Feature.Coefficient
import           CV.Filter.PolarSeparableFilter
import           CV.Utility.Parallel                   as Parallel
import           Data.Array.Repa                       as R
import           Data.Binary
import           Data.ByteString.Lazy                  as BL
import           Data.Conduit
import           Data.Conduit.Binary                   as CB
import           Data.Conduit.List                     as CL
import           Data.List                             as L
import           Data.Set                              as S
import           Prelude                               as P
import           System.Directory
import           System.Environment
import           System.IO                             as IO

main = do
  args <- getArgs
  when (P.null args) $ error "run with --help to see options."
  params <- parseArgs args
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
        , getRadialFreqSet = S.fromDistinctAscList [1 .. (freq' - 0)]
        , getAngularFreqSet = S.fromDistinctAscList [0 .. (freq' - 1)]
        , getNameSet = Pinwheels
        }
      filterParamsSetList =
        L.zipWith filterParamsSetFunc [1] (freq params)
      numM = numGaussian params
      magnitudeConduit filterParams =
        if isFixedSize params
          then let (PolarSeparableFilter _ filter') = makeFilterSet filterParams
                   (PolarSeparableFilter _ flippedFilter') =
                     makeFlippedFilterSet filterParams
               in coefficientMagnitudeConduit
                    parallelParams
                    (learningRate params)
                    filter'
                    flippedFilter'
                    False
          else undefined
      imgArrs = L.map (\(LabeledArray _ arr) -> arr) images
  print params
  withBinaryFile (pcaFile params) WriteMode $
    \h ->
       M.foldM
         (\arrs (filterParamsSet, np, downsampleFactor') -> do
            let filter' = makeFilterSet
            runResourceT $
              sourceList arrs $$ magnitudeConduit filterParamsSet =$=
              hPCASink parallelParams h (numGMMExample params) np downsampleFactor')
         imgArrs $
       L.zip3
         filterParamsSetList
         (numPrincipal params)
         (downsampleFactor params)
