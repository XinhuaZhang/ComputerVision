import           Application.RotatedMNIST.ArgsParser as AP
import           Application.RotatedMNIST.Conduit
import           Control.Monad                       as M
import           Control.Monad.Trans.Resource
import           CV.Array.LabeledArray
import           CV.Filter.FourierMellinTransform
import           CV.Filter.ShiftablePinwheelPyramid
import           CV.Statistics.KMeans
import           CV.Statistics.PCA
import           CV.Utility.Parallel                 as Par
import           Data.Array.Repa
import           Data.Binary
import           Data.Conduit
import           Data.Conduit.Binary                 as CB
import           Data.Conduit.List                   as CL
import           Data.List                           as L
import           System.Environment

main = do
  args <- getArgs
  params <- parseArgs args
  print params
  ((LabeledArray _ img):_) <-
    runResourceT $
    CB.sourceFile (inputFile params) $$ readLabeledImagebinaryConduit =$=
    CL.take 1
  let parallelParams =
        ParallelParams
        { Par.numThread = AP.numThread params
        , Par.batchSize = AP.batchSize params
        }
      (Z :. nf :. _ :. _) = extent img
      filterParams =
        ShiftablePinwheelBlobPyramidParams
        { shiftablePinwheelBlobPyramidNumLayers = 2
        , shiftablePinwheelBlobPyramidNumCenters = (numGrid params) ^ (2 :: Int)
        , shiftablePinwheelBlobPyramidNumChannels = nf
        , shiftablePinwheelBlobPyramidNumTheta = 64
        , shiftablePinwheelBlobPyramidNumLogR = 32
        , shiftablePinwheelBlobPyramidK = 7
        }
      filters =
        generateShiftablePinwheelBlobPyramidScatteringNetworksFilters
          filterParams
  writeFile (paramsFileName params) . show $ filterParams
  importFFTWWisdom (fftwWisdomPath params)
  originPredictorParams <-
    fmap (\x -> read x :: FourierMellinTransformParamsGrid) . readFile $
    (originPredictorParamsFileName params)
  (plan0, originPredictorFilters) <-
    makeFilterConvolution getEmptyPlan originPredictorParams Normal :: IO (DFTPlan, FourierMellinTransformConvolution)
  plan <- shiftablePinwheelBlobPyramidScatteringNetworksPlan plan0 filters
  exportFFTWWisdom (fftwWisdomPath params)
  -- (xs:_) <-
  --   runResourceT $
  --   CB.sourceFile (inputFile params) $$ readLabeledImagebinaryConduit =$=
  --   -- remove9Conduit =$=
  --   getOriginsConduit
  --     (ParallelParams 1 1)
  --     plan
  --     [originPredictorFilters]
  --     (originModelName params) =$=
  --   logpolarImageConduit
  --     (ParallelParams 1 1)
  --     (shiftablePinwheelBlobPyramidNumTheta filterParams)
  --     (shiftablePinwheelBlobPyramidNumLogR filterParams)
  --     (radius params)
  --     (logpolarFlag params)
  --     (numGrid params) =$=
  --   shiftablePinwheelBlobPyramidScatteringNetworksConduit
  --     (ParallelParams 1 1)
  --     plan
  --     (numScatteringLayer params)
  --     (stride params)
  --     (shiftablePinwheelBlobPyramidNumLayers filterParams)
  --     (shiftablePinwheelBlobPyramidK filterParams)
  --     filters
  --     Nothing =$=
  --   CL.take 1
  -- origins <-
  --   runResourceT $
  --   CB.sourceFile (inputFile params) $$ readLabeledImagebinaryConduit =$=
  --                                           -- remove9Conduit =$=
  --   getOriginsConduit
  --     parallelParams
  --     plan
  --     [originPredictorFilters]
  --     (originModelName params) =$=
  --   CL.map fst =$=
  --   CL.take (numGMMExample params)
  -- kmeansModel <-
  --   M.mapM
  --     (\n ->
  --        runResourceT $
  --        CB.sourceFile (inputFile params) $$ readLabeledImagebinaryConduit =$=
  --                                          -- remove9Conduit =$=
  --        mergeSource (CL.sourceList origins) =$=
  --        logpolarImageConduit
  --          parallelParams
  --          (shiftablePinwheelBlobPyramidNumTheta filterParams)
  --          (shiftablePinwheelBlobPyramidNumLogR filterParams)
  --          (radius params)
  --          (logpolarFlag params)
  --          (numGrid params) =$=
  --        shiftablePinwheelBlobPyramidScatteringNetworksConduit
  --          parallelParams
  --          plan
  --          (numScatteringLayer params)
  --          (stride params)
  --          (shiftablePinwheelBlobPyramidNumLayers filterParams)
  --          (shiftablePinwheelBlobPyramidK filterParams)
  --          filters
  --          (Just n) =$=
  --        kmeansSink
  --          parallelParams
  --          (numGMMExample params)
  --          (numGaussian params)
  --          (kmeansFile params)
  --          (threshold params))
  --     [0 .. (L.length . snd $ xs) - 1]
  xs <-
    runResourceT $
    CB.sourceFile (inputFile params) $$ readLabeledImagebinaryConduit =$=
    getOriginsConduit
      parallelParams
      plan
      [originPredictorFilters]
      (originModelName params) =$=
    logpolarImageConduit
      parallelParams
      (shiftablePinwheelBlobPyramidNumTheta filterParams)
      (shiftablePinwheelBlobPyramidNumLogR filterParams)
      (radius params)
      (logpolarFlag params)
      (numGrid params) =$=
    shiftablePinwheelBlobPyramidScatteringNetworksConduit
      parallelParams
      plan
      (numScatteringLayer params)
      (stride params)
      (shiftablePinwheelBlobPyramidNumLayers filterParams)
      (shiftablePinwheelBlobPyramidK filterParams)
      filters
      Nothing =$=
    CL.take (numGMMExample params)
  kmeansModel <-
    M.mapM
      (kmeans
         parallelParams
         (numGaussian params)
         (kmeansFile params)
         (threshold params))
      (L.map L.concat . L.transpose . snd . L.unzip $ xs)
  encodeFile (kmeansFile params) kmeansModel
  zs <-
    runResourceT $
    sourceList xs $$ kmeansConduit parallelParams kmeansModel =$= CL.consume
  let (pcaMat, eigValVec, _) =
        pcaSVD parallelParams (numPrincipal params) . snd . L.unzip $ zs
  print eigValVec
  encodeFile (pcaFile params) pcaMat
