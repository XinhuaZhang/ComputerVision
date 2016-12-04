module Main where

import           Application.GMM.ArgsParser         as Parser
import           Application.GMM.FisherKernel
import           Application.GMM.GMM
import           Application.GMM.MixtureModel
import           Classifier.LibLinear
import           Control.Monad
import           Control.Monad.IO.Class
import qualified Control.Monad.Parallel             as MP
import           Control.Parallel
import           CV.Array.LabeledArray
import           CV.Feature.PolarSeparableRepa
import           CV.Filter
import           CV.Filter.PolarSeparableFilter
import           CV.Filter.PolarSeparableFilterRepa
import           CV.IO.ImageIO
import           CV.Utility.Parallel                as Parallel
import           CV.Utility.Time
import           Data.Array.Repa                    as R
import           Data.Binary
import           Data.Complex                       as C
import           Data.Conduit
import           Data.Conduit.List                  as CL
import           Data.List                          as L
import           Data.Maybe                         as Maybe
import           Data.Set                           as S
import           Data.Time.LocalTime
import           Data.Vector                        as V
import           Data.Vector.Unboxed                as VU
import           Foreign.Ptr
import           Prelude                            as P
import           System.Environment


sliceConduit :: ParallelParams
             -> Conduit (LabeledArray DIM3 Double) IO (Int, [VU.Vector Double])
sliceConduit parallelParams =
  do xs <- CL.take (Parallel.batchSize parallelParams)
     unless (P.null xs)
            (do let ys =
                      parMapChunk
                        parallelParams
                        rdeepseq
                        (\(LabeledArray label arr) ->
                           let (Z :. nf :. ny :. nx) = extent arr
                           in (label
                              ,P.map (\i ->
                                        toUnboxed . computeS $
                                        R.slice arr (Z :. i :. All :. All)) $
                               [0 .. nf - 1]))
                        xs
                sourceList ys
                sliceConduit parallelParams)

trainSink
  :: ParallelParams -> FilePath -> TrainParams -> Bool -> Sink (Int,VU.Vector Double) IO ()
trainSink parallelParams filePath trainParams findCFlag = do
  --xs <- consume
  -- if ((VU.length . P.head $ xs) /= (trainFeatureIndexMax trainParams))
  --   then error $
  --        "Number of feature in trainParams is not correct. (" P.++
  --        (show . VU.length . P.head $ xs) P.++
  --        " vs " P.++
  --        (show $ trainFeatureIndexMax trainParams) P.++
  --        ")"
  --   else return ()
  -- featurePtrs <-
  --   xs `pseq` liftIO $ MP.mapM (getFeatureVecPtr . Dense . VU.toList) xs
  -- label <- liftIO $ readLabelFile filePath
  -- if findCFlag
  --   then liftIO $ findParameterC trainParams label featurePtrs
  --   else liftIO $ train trainParams label $
  go [] []
  where
    go :: [[Double]] -> [[Ptr C'feature_node]] -> Sink (Int,VU.Vector Double) IO ()
    go label pss = do
      xs <- CL.take (Parallel.batchSize parallelParams)
      if P.length xs > 0
        then do
          let (ls,ys) = P.unzip xs
          ps <- liftIO $ P.mapM (getFeatureVecPtr . Dense . VU.toList) ys
          liftIO $ printCurrentTime
          go ((P.map fromIntegral ls) : label)  $! (ps : pss)
        else liftIO $ train trainParams (P.concat . L.reverse $ label) (P.concat . L.reverse $ pss)

main = do
  args <- getArgs
  if P.null args
    then error "run with --help to see options."
    else return ()
  params <- parseArgs args
  gmm <- readGMM (gmmFile params) :: IO [GMM]
  imageListLen <- getArrayNumFile (inputFile params)
  let parallelParams =
        ParallelParams
        { Parallel.numThread = Parser.numThread params
        , Parallel.batchSize = Parser.batchSize params
        }
      filterParams =
        PolarSeparableFilterParams
        { getRadius = 128
        , getScale = S.fromDistinctAscList (scale params)
        , getRadialFreq = S.fromDistinctAscList [0 .. (freq params - 1)]
        , getAngularFreq = S.fromDistinctAscList [0 .. (freq params - 1)]
        , getName = Pinwheels
        }
      trainParams =
        TrainParams
        { trainSolver = L2R_L2LOSS_SVC_DUAL
        , trainC = c params
        , trainNumExamples = imageListLen
        , trainFeatureIndexMax =
          if isComplex params
            then (4 * getFilterNum filterParams) * (numModel $ P.head gmm)
            else (2 * getFilterNum filterParams) * (numModel $ P.head gmm)
        , trainModel = modelName params
        }
      filters =
        makeFilter filterParams :: PolarSeparableFilter (R.Array U DIM3 (C.Complex Double))
  print params
  readLabeledImagebinarySource (inputFile params) $$
    magnitudeLabeledArrayConduit'
      parallelParams
      filters
      (downsampleFactor params) =$=
  -- CL.map
  --   (\(LabeledArray label arr) ->
  --       let (Z :. nf :. ny :. nx) = extent arr
  --           r = (fromIntegral $ nx ^ 2 + ny ^ 2) / 4
  --           centerX = fromIntegral nx / 2
  --           centerY = fromIntegral ny / 2
  --           vec =
  --             P.map
  --               (\(a, b) ->
  --                   if (fromIntegral b - centerX) ^ 2 + (fromIntegral a - centerY) ^ 2 < r
  --                     then Just . toUnboxed . computeS $
  --                          R.slice arr (Z :. All :. a :. b)
  --                     else Nothing) $
  --             [ (i, j)
  --             | i <- [0 .. ny - 1]
  --             , j <- [0 .. nx - 1] ]
  --       in (label, V.fromList . Maybe.catMaybes $ vec))
    sliceConduit parallelParams =$=
    (fisherVectorConduit parallelParams gmm) =$=
    trainSink parallelParams (labelFile params) trainParams (findC params)
