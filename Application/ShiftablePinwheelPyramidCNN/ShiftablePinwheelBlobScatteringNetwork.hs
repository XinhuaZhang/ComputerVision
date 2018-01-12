import           Application.RotatedMNIST.ArgsParser             as AP
import           Application.ShiftablePinwheelPyramidCNN.Conduit
import           Control.Monad                                   as M
import           Control.Monad.Trans.Resource
import           CV.Array.LabeledArray
import           CV.Filter.FourierMellinTransform
import           CV.Filter.ShiftablePinwheelPyramid
import           CV.Utility.Parallel                             as Par
import           Data.Array.Repa
import           Data.Binary
import           Data.Conduit
import           Data.Conduit.Binary                             as CB
import           Data.Conduit.List                               as CL
import           Data.List                                       as L
import           System.Environment
import           System.FilePath

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
        , shiftablePinwheelBlobPyramidNumLogR = 64
        , shiftablePinwheelBlobPyramidK = 1
        }
      filters =
        generateShiftablePinwheelBlobPyramidScatteringNetworksFilters
          filterParams
  writeFile (paramsFileName params) . show $ filterParams
  -- originPredictorParams <-
  --   fmap (\x -> read x :: FourierMellinTransformParamsGrid) . readFile $
  --   (originPredictorParamsFileName params)
  -- (plan0, originPredictorFilters) <-
  --   makeFilterConvolution getEmptyPlan originPredictorParams Normal :: IO (DFTPlan, FourierMellinTransformConvolution)
  plan <- shiftablePinwheelBlobPyramidScatteringNetworksPlan getEmptyPlan filters
  runResourceT $
    CB.sourceFile (inputFile params) $$ readLabeledImagebinaryConduit =$=
    -- getOriginsConduit
    --   parallelParams
    --   plan
    --   [originPredictorFilters]
    --   (originModelName params) =$=
    centerConduit =$=
    logpolarImageConduit
      parallelParams
      (shiftablePinwheelBlobPyramidNumTheta filterParams)
      (shiftablePinwheelBlobPyramidNumLogR filterParams)
      (radius params)
      (logpolarFlag params)
      (numGrid params) =$=
    shiftablePinwheelBlobPyramidScatteringNetworkCNNConduit
      parallelParams
      plan
      filters
      (numScatteringLayer params)
      (shiftablePinwheelBlobPyramidNumLayers filterParams)
      (shiftablePinwheelBlobPyramidK filterParams) =$=
    hdf5Sink
      parallelParams
      (("BlobScattering_" L.++ show (shiftablePinwheelBlobPyramidK filterParams) L.++
        "_" L.++
        show (shiftablePinwheelBlobPyramidNumLayers filterParams)) </>
       (takeBaseName . inputFile $ params))
