module Main where

import           Application.GMM.ArgsParser     as Parser
import           Application.GMM.GMM
import           Application.GMM.PCA
import           Control.Monad                  as M
import           Control.Monad.Trans.Resource
import           CV.Array.LabeledArray
import           CV.Feature.PolarSeparableRepa
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
      filterParamsSetList = [filterParamsSet1, filterParamsSet2]
      numM = numGaussian params
      bound = ((0, 10), (0.1, 100))
      pcaMagnitudeConduit =
        if isFixedSize params
          then magnitudeSetFixedSizeConduit
                 parallelParams
                 (L.map makeFilterSet filterParamsSetList)
                 (downsampleFactor params)
          else magnitudeSetVariedSizeConduit
                 parallelParams
                 filterParamsSetList
                 (downsampleFactor params)
  print params
  pcaMatrix <-
    runResourceT $
    sourceFile (inputFile params) $$ readLabeledImagebinaryConduit =$= pcaMagnitudeConduit =$=
    pcaSink (numGMMExample params) (numPrincipal params)
  writeMatrix (pcaFile params) pcaMatrix
