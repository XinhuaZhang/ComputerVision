module Main where

import           Application.GMM.ArgsParser     as Parser
import           Application.GMM.GMM
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

splitList :: Bool -> Int -> [a] -> [[a]]
splitList isColor n xs
  | P.null xs = []
  | otherwise = as : splitList isColor n bs
  where
    (as, bs) =
      if isColor
        then P.splitAt (3 * n) xs
        else P.splitAt n xs

main = do
  args <- getArgs
  if P.null args
    then error "run with --help to see options."
    else return ()
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
  isColor <-
    do xs <-
         runResourceT $
         sourceFile (inputFile params) $$ readLabeledImagebinaryConduit =$=
         CL.take 1
       let (LabeledArray _ arr) = L.head xs
           (Z :. nf :. _ :. _) = extent arr
       case nf of
         3 -> return True
         1 -> return False
         _ -> error $ "Images have incorrect number of channels: " P.++ show nf
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
        , getDownsampleFactorSet = 1
        , getScaleSet = S.fromDistinctAscList (scale params)
        , getRadialFreqSet = S.fromDistinctAscList [0 .. (freq params - 1)]
        , getAngularFreqSet = S.fromDistinctAscList [0 .. (freq params - 1)]
        , getNameSet = Pinwheels
        }
      filterParamsSetList = [filterParamsSet1, filterParamsSet1]
      filterParamsList =
        splitList False (Parallel.batchSize parallelParams) .
        P.concatMap generateMultilayerPSFParamsSet . L.tail . L.inits $
        filterParamsSetList
      filePath = gmmFile params
      numM = numGaussian params
      bound = ((0, 100), (100, 10000))
      numFeature =
        if isColor
          then 3 * (P.sum . P.map P.length $ filterParamsList)
          else P.sum . P.map P.length $ filterParamsList
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
  let gmmsList = splitList isColor (Parallel.batchSize parallelParams) gmms
      magnitudeConduit paramsList =
        if isFixedSize params
          then magnitudeFixedSizeConduit
                 parallelParams
                 (L.map (L.map makeFilter) paramsList)
                 (downsampleFactor params)
          else magnitudeVariedSizeConduit
                 parallelParams
                 paramsList
                 (downsampleFactor params)
  images <- readLabeledImageBinary (inputFile params) (numGMMExample params)
  withBinaryFile
    (gmmFile params)
    WriteMode
    (\h -> do
       BL.hPut h (encode (fromIntegral numFeature :: Word32))
       M.foldM_
         (\handle (models, filterParams) ->
             runResourceT
               (CL.sourceList images $$ CL.map (\(LabeledArray _ arr) -> arr) =$=
                magnitudeConduit filterParams =$=
                gmmPartSink
                  handle
                  models
                  bound
                  (threshold params)
                  (numGMMExample params)))
         h $
         P.zip gmmsList filterParamsList)
