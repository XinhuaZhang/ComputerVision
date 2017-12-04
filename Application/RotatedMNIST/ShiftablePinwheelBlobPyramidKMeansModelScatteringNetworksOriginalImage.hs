import           Application.RotatedMNIST.ArgsParser as AP
import           Application.RotatedMNIST.Conduit
import           Control.Monad                       as M
import           Control.Monad.Trans.Resource
import           CV.Array.LabeledArray
import           CV.Filter.FourierMellinTransform
import           CV.Filter.ShiftablePinwheelPyramid
import           CV.Statistics.KMeans
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
        , shiftablePinwheelBlobPyramidNumTheta = imageSize params
        , shiftablePinwheelBlobPyramidNumLogR = imageSize params
        , shiftablePinwheelBlobPyramidK = 17
        }
      filters =
        generateShiftablePinwheelBlobPyramidScatteringNetworksFilters
          filterParams
  writeFile (paramsFileName params) . show $ filterParams
  plan <-
    shiftablePinwheelBlobPyramidScatteringNetworksPlan getEmptyPlan filters
  exportFFTWWisdom (fftwWisdomPath params)
  xs <-
    runResourceT $
    CB.sourceFile (inputFile params) $$ readLabeledImagebinaryConduit =$=
    imageConduit parallelParams =$=
    shiftablePinwheelBlobPyramidScatteringNetworksConduit
      parallelParams
      plan
      (numScatteringLayer params)
      (stride params)
      (shiftablePinwheelBlobPyramidNumLayers filterParams)
      (shiftablePinwheelBlobPyramidK filterParams)
      filters
      Nothing =$=
    concatConduit parallelParams =$=
    CL.take (numGMMExample params)
  let (ls, ys) = L.unzip xs
  kmeansModel <-
    M.mapM
      (kmeans parallelParams (numGaussian params) (kmeansFile params) 0.005)
      (L.map L.concat . L.transpose $ ys)
  -- kmeansModel <-
  --   M.mapM
  --     (\n ->
  --        runResourceT $
  --        CB.sourceFile (inputFile params) $$ readLabeledImagebinaryConduit =$=
  --        imageConduit parallelParams =$=
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
  encodeFile (kmeansFile params) kmeansModel
