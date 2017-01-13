{-# LANGUAGE BangPatterns #-}
module Main where

import           Application.FilterStats.FilterStats
import           Control.Monad as M
import           CV.Array.LabeledArray
import           CV.Feature.PolarSeparableRepa
import           CV.Filter.PolarSeparableFilter
import           CV.Utility.Parallel                 as Parallel
import           Data.List                           as L
import           Data.Set                            as S
import           Data.Vector.Unboxed                 as VU
import           System.Environment

splitList :: Int -> [a] -> [[a]]
splitList n xs
  | L.null xs = []
  | otherwise = as : splitList n bs
  where (as,bs) = L.splitAt n xs
  
printParams :: [PolarSeparableFilterParams] -> String
printParams =
  L.concatMap
    (\(PolarSeparableFilterParams s df scale rf af name) ->
        "r" L.++ show rf L.++ "a" L.++ show af)

main = do
  (inputFile:_) <- getArgs
  arrList <- readLabeledImagebinary inputFile
  let downsampleFactor = 8
      nBins = 100
      parallelParams =
        ParallelParams
        { Parallel.numThread = 4
        , Parallel.batchSize = 800
        }
      filterParamsSet1 =
        PolarSeparableFilterParamsSet
        { getSizeSet = (0, 0)
        , getDownsampleFactorSet = 1
        , getScaleSet = S.fromDistinctAscList [8]
        , getRadialFreqSet = S.fromDistinctAscList [0 .. (4 - 1)]
        , getAngularFreqSet = S.fromDistinctAscList [0 .. (4 - 1)]
        , getNameSet = Pinwheels
        }
      filterParamsSet2 =
        PolarSeparableFilterParamsSet
        { getSizeSet = (0, 0)
        , getDownsampleFactorSet = 2
        , getScaleSet = S.fromDistinctAscList [4]
        , getRadialFreqSet = S.fromDistinctAscList [0 .. (4 - 1)]
        , getAngularFreqSet = S.fromDistinctAscList [0 .. (4 - 1)]
        , getNameSet = Pinwheels
        }
      filterParamsSetList = [filterParamsSet1, filterParamsSet2]
      filterParamsList =
        splitList (numThread parallelParams) .
        L.concatMap generateMultilayerPSFParamsSet . L.tail . L.inits $
        filterParamsSetList
  M.mapM_
    (\filterParams -> do
       let !xs =
             parMapChunk
               parallelParams
               rdeepseq
               (\(LabeledArray _ arr) ->
                   L.concatMap
                     (\params ->
                         multiLayerMagnitudeVariedSize params downsampleFactor arr)
                     filterParams)
               $ L.take 1 arrList
           !ys = L.map VU.concat . L.transpose $ xs
           !muSigma = L.map meanVar ys
           !range = L.map valueRange ys
       M.sequence_ $
         L.zipWith4
           (\y (mu, sigma) (a, b) params ->
               plotHist
                 y
                 (a, b)
                 nBins
                 (show (mu, sigma) L.++ " " L.++
                  printParams params)
                 (printParams params L.++ ".png"))
           ys
           muSigma
           range
           filterParams)
    filterParamsList
