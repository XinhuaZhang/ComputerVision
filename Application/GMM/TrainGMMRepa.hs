module Main where

import           Application.GMM.ArgsParser     as Parser
import           Application.GMM.GMM
import           Control.Monad
import           Control.Monad                  as M
import           CV.Array.LabeledArray
import           CV.Feature.PolarSeparableRepa
import           CV.Filter
import           CV.Filter.PolarSeparableFilter
import           CV.IO.ImageIO
import           CV.Utility.Parallel            as Parallel
import           Data.Array.Repa                as R
import           Data.Binary
import           Data.ByteString.Lazy           as BL
import           Data.Complex
import           Data.Conduit
import           Data.Conduit.List              as CL
import           Data.Set                       as S
import           Data.Vector                    as V
import           Data.Vector.Unboxed            as VU
import           Prelude                        as P
import           System.Directory
import           System.Environment
import           System.IO                      as IO

splitList :: Int -> [a] -> [[a]]
splitList n xs
  | P.null xs = []
  | otherwise = as : splitList n bs
  where (as,bs) = P.splitAt n xs


main = do
  args <- getArgs
  if P.null args
    then error "run with --help to see options."
    else return ()
  params <- parseArgs args
  let parallelParams =
        ParallelParams
        { Parallel.numThread = Parser.numThread params
        , Parallel.batchSize = Parser.batchSize params
        }
      filterParamsSet1 =
        PolarSeparableFilterParamsSet
        { getSizeSet = (0, 0)
        , getDowsampleFactorSet = 1
        , getScaleSet = S.fromDistinctAscList (scale params)
        , getRadialFreqSet = S.fromDistinctAscList [0 .. (freq params - 1)]
        , getAngularFreqSet = S.fromDistinctAscList [0 .. (freq params - 1)]
        , getNameSet = Pinwheels
        }
      filterParamsSet2 =
        PolarSeparableFilterParamsSet
        { getSizeSet = (0, 0)
        , getDowsampleFactorSet = 2
        , getScaleSet = S.fromDistinctAscList (scale params)
        , getRadialFreqSet = S.fromDistinctAscList [0 .. (freq params - 1)]
        , getAngularFreqSet = S.fromDistinctAscList [0 .. (freq params - 1)]
        , getNameSet = Pinwheels
        }
      filterParamsList =
        splitList (Parser.numThread params) $
        generateMultilayerPSFParamsSet [filterParamsSet1, filterParamsSet2]
      filePath = gmmFile params
      numM = numGaussian params
      bound = ((0, 10), (0.1, 100))
      numLayer = 2
      numFeature = P.sum . P.map P.length $ filterParamsList
  print params
  fileFlag <- doesFileExist filePath
  gmms <-
    if fileFlag
      then do
        fileSize <- getFileSize filePath
        if fileSize > 0
          then do
            IO.putStrLn $ "Read GMM data file: " P.++ filePath
            readGMM filePath
          else M.replicateM numFeature $ initializeGMM numM bound
      else M.replicateM numFeature $ initializeGMM numM bound
  let gmmsList = splitList (Parser.numThread params) gmms
  withBinaryFile
    (gmmFile params)
    WriteMode
    (\h -> do
       BL.hPut h (encode (fromIntegral numFeature :: Word32))
       M.foldM_
         (\handle (models, filterParams) ->
             readLabeledImagebinarySource (inputFile params) $$
             CL.map (\(LabeledArray _ arr) -> arr) =$=
             magnitudeVariedSizeConduit
               parallelParams
               filterParams
               (downsampleFactor params) =$=
             gmmSink2 parallelParams handle models bound (threshold params))
         h $
         P.zip gmmsList filterParamsList)
