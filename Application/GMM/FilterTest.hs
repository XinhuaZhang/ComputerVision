{-# LANGUAGE BangPatterns #-}
module Main where

import           Application.GMM.ArgsParser     as Parser
import           Application.GMM.GMM
import           Control.Monad                  as M
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           CV.Array.Image
import           CV.Array.LabeledArray
import           CV.Feature.PolarSeparable
import           CV.Filter
import           CV.Filter.PolarSeparableFilter
import           CV.IO.ImageIO
import           CV.Utility.Parallel            as Parallel
import qualified CV.Utility.RepaArrayUtility    as RU
import           Data.Array.Repa                as R
import           Data.Complex
import           Data.Conduit
import           Data.Conduit.Binary            as CB
import           Data.Conduit.List              as CL
import           Data.Image                     as IM
import           Data.List                      as L
import           Data.Set                       as S
import           Data.Vector                    as V
import           Data.Vector.Unboxed            as VU
import           Prelude                        as P
import           System.Environment
import qualified Data.Array                     as Arr


correctSink
  :: ParallelParams
  -> PolarSeparableFilterParamsSet
  -> Sink (LabeledArray DIM3 Double) (ResourceT IO) [VU.Vector  Double]
correctSink parallelParams paramsSet = do
  xs <- CL.take 1
  let !(LabeledArray label arr) = L.head xs
      !(Z :. nf' :. ny' :. nx') = extent arr
      !paramsList = generatePSFParamsSet paramsSet
      !ys =
        parMapChunk
          parallelParams
          rseq
          (\p@(PolarSeparableFilterParams _ downsampleFactor scale rf af name) ->
              let !filter' =
                    IM.makeFilter ny' nx' (getFilterFunc p scale rf af) :: ComplexImage
                  !img =
                    makeImage ny' nx' (\j i -> arr R.! (Z :. 0 :. j :. i)) :: GrayImage
                  !result =
                    IM.magnitude $ IM.ifft ((IM.fft filter') * (IM.fft img))
              in result)
          paramsList
  liftIO $
    M.zipWithM_
      (\i y -> writeImage (show i L.++ "_correct.pgm") y)
      [1 ..]
      ys
  return $! L.map (VU.fromList . pixelList) ys

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
        { getSizeSet = (64, 128)
        , getDownsampleFactorSet = 1
        , getScaleSet = S.fromDistinctAscList (scale params)
        , getRadialFreqSet = S.fromDistinctAscList [0 .. (freq params - 1)]
        , getAngularFreqSet = S.fromDistinctAscList [0 .. (freq params - 1)]
        , getNameSet = Pinwheels
        }
      filterParamsSetList = [filterParamsSet]
      n = 128
  xs <-
    runResourceT $
    sourceFile (inputFile params) $$ readLabeledImagebinaryConduit =$=
    CL.map (\(LabeledArray _ arr) -> RU.crop [0, 0, 0] [n, div n 2, 1] arr) =$=
    singleLayerMagnitudeVariedSizedConduit
      parallelParams
      filterParamsSet
      (downsampleFactor params) =$=
    CL.take 1
  ys <-
    runResourceT $
    sourceFile (inputFile params) $$ readLabeledImagebinaryConduit =$=
    CL.map
      (\(LabeledArray label arr) ->
          LabeledArray label $ computeS $ RU.crop [0, 0, 0] [n, div n 2, 1] arr) =$=
    multiLayerMagnitudeVariedSizedConduit
      parallelParams
      filterParamsSetList
      (downsampleFactor params) =$=
    CL.take 1
  z <-
    runResourceT $
    sourceFile (inputFile params) $$ readLabeledImagebinaryConduit =$=
    CL.map
      (\(LabeledArray label arr) ->
          LabeledArray label $ computeS $ RU.crop [0, 0, 0] [n, div n 2, 1] arr) =$=
    correctSink parallelParams filterParamsSet
  let a =
        toUnboxed .
        --(\arr -> computeS . R.slice arr $ (Z :. (0 :: Int) :. All :. All)) .
        L.head $
        xs
      b = VU.concat . L.concat . snd . L.head $ ys
      c = VU.concat z
  print . VU.maximum . VU.map abs . VU.zipWith (-) a $ c
  print . VU.sum . VU.zipWith (-) b $ c
  M.mapM_
    (\i ->
        plotImage (show i L.++ "_test.png") .
        computeS . extend (Z :. (1 :: Int) :. All :. All) . R.slice (L.head xs) $
        (Z :. (i - 1 :: Int) :. All :. All))
    [1 .. (freq params) ^ 2]
  let paramsList = generatePSFParamsSet filterParamsSet
      (PolarSeparableFilter _ filter'') = makeFilterSet filterParamsSet
  M.zipWithM_
    (\i p@(PolarSeparableFilterParams _ downsampleFactor scale rf af name) ->
        let !filter' = IM.makeFilter 64 128 (getFilterFunc p scale rf af) :: ComplexImage
        in writeImage (show i L.++ "_correct_filter.ppm") filter')
    [1 ..]
    paramsList
  M.mapM_
    (\i ->
        let arr =
              Arr.listArray ((0, 0), (63, 127)) . R.toList . R.slice filter'' $
              (Z :. (i - 1 :: Int) :. All :. All)
        in writeImage
             (show i L.++ "_test_filter.ppm")
             (arrayToImage arr :: ComplexImage))
    [1 .. (freq params) ^ 2]
