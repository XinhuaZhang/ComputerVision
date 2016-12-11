{-# LANGUAGE BangPatterns #-}
module Main where

import           Application.GMM.ArgsParser     as Parser
import           Application.GMM.GMM
import           Control.Monad                  as M
import           CV.Array.Image
import           CV.Array.LabeledArray
import           CV.Feature.PolarSeparableRepa
import           CV.Filter
import           CV.Filter.PolarSeparableFilter
import           CV.IO.ImageIO
import           CV.Utility.Parallel            as Parallel
import           Data.Array.Repa                as R
import           Data.Complex
import           Data.Conduit
import           Data.Conduit.List              as CL
import           Data.List                      as L
import           Data.Set                       as S
import           Data.Vector                    as V
import           Data.Vector.Unboxed            as VU
import           Prelude                        as P
import           System.Environment
import Data.Image as IM
import qualified CV.Utility.RepaArrayUtility as RU


correctSink
  :: ParallelParams
  -> PolarSeparableFilterParamsSet
  -> Sink (LabeledArray DIM3 Double) IO [VU.Vector  Double]
correctSink parallelParams paramsSet = do
  xs <- CL.take 1
  let !(LabeledArray label arr) = L.head xs
      !(Z :. nf' :. ny' :. nx') = extent arr
      !paramsList = generatePSFParamsSet paramsSet
      !ys =
        parMapChunk
          parallelParams
          rdeepseq
          (\p@(PolarSeparableFilterParams _ downsampleFactor scale rf af name) ->
              let !filter' = IM.makeFilter ny' nx' (getFilterFunc p scale rf af) :: ComplexImage
                  !img = makeImage ny' nx' (\i j -> arr R.! (Z :. 0 :. i :. j)) :: GrayImage
                  !result = IM.magnitude $ IM.ifft ((IM.fft filter') * (IM.fft img))
              in VU.fromList . pixelList $ result)
          paramsList
  return ys

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
      filterParamsSet =
        PolarSeparableFilterParamsSet
        { getSizeSet = (0, 0)
        , getDownsampleFactorSet = 1
        , getScaleSet = S.fromDistinctAscList (scale params)
        , getRadialFreqSet = S.fromDistinctAscList [0 .. (freq params - 1)]
        , getAngularFreqSet = S.fromDistinctAscList [0 .. (freq params - 1)]
        , getNameSet = Pinwheels
        }
      filterParamsSetList = [filterParamsSet]
      filterParamsList =
        P.concatMap generateMultilayerPSFParamsSet . L.tail . L.inits $ filterParamsSetList
  xs <-
    readLabeledImagebinarySource (inputFile params) $$
    CL.map
      (\(LabeledArray _ arr) -> computeS $ RU.crop [0, 0, 0] [256, 256, 1] arr) =$=
    magnitudeVariedSizeConduit
      parallelParams
      filterParamsList
      (downsampleFactor params) =$=
    CL.take 1
  ys <-
    readLabeledImagebinarySource (inputFile params) $$
    CL.map
      (\(LabeledArray label arr) ->
          LabeledArray label $ computeS $ RU.crop [0, 0, 0] [256, 256, 1] arr) =$=
    labeledArrayMagnitudeSetVariedSizeConduit
      parallelParams
      filterParamsSetList
      (downsampleFactor params) =$=
    CL.take 1
  z <-
    readLabeledImagebinarySource (inputFile params) $$
    CL.map
      (\(LabeledArray label arr) ->
          LabeledArray label $ computeS  $ RU.crop [0, 0, 0] [256, 256, 1] arr) =$=
    correctSink parallelParams filterParamsSet
  M.mapM_ (\(a, b, c) ->
             do let n = 4095
                print (c VU.! n , a VU.! n, b VU.! n)) $
    L.zip3 (L.head xs) (snd . L.head $ ys) z
  -- M.mapM_
  --   (\(a, b) -> do
  --      print . VU.length $ a
  --      print . VU.length $ b
  --      print (a VU.! 1000, b VU.! 1000)) $
  --   L.zip (L.head xs) (snd . L.head $ ys)
