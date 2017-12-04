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
        , shiftablePinwheelBlobPyramidNumTheta = 32
        , shiftablePinwheelBlobPyramidNumLogR = 32
        , shiftablePinwheelBlobPyramidK = 6
        }
      filters = generateShiftablePinwheelBlobPyramidFilters filterParams
  writeFile (paramsFileName params) . show $ filterParams
  importFFTWWisdom (fftwWisdomPath params)
  originPredictorParams <-
    fmap (\x -> read x :: FourierMellinTransformParamsGrid) . readFile $
    (originPredictorParamsFileName params)
  (plan0, originPredictorFilters) <-
    makeFilterConvolution getEmptyPlan originPredictorParams Normal :: IO (DFTPlan, FourierMellinTransformConvolution)
  ((_, x):_) <-
    runResourceT $
    CB.sourceFile (inputFile params) $$ readLabeledImagebinaryConduit =$=
    getOriginsConduit
      parallelParams
      plan0
      [originPredictorFilters]
      (originModelName params) =$=
    logpolarImageConduit
      parallelParams
      (shiftablePinwheelBlobPyramidNumTheta filterParams)
      (shiftablePinwheelBlobPyramidNumLogR filterParams)
      (radius params)
      (logpolarFlag params)
      (numGrid params) =$=
    CL.take 1
  plan <- shiftablePinwheelBlobPyramidPlan plan0 filters x
  exportFFTWWisdom (fftwWisdomPath params)
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
    shiftablePinwheelBlobPyramidConduit
      parallelParams
      plan
      (stride params)
      filters =$=
    CL.take (numGMMExample params)
  let (ls, ys) = L.unzip xs
  kmeansModel <-
    M.mapM
      (kmeans parallelParams (numGaussian params) (kmeansFile params) 0.005)
      (L.map L.concat . L.transpose $ ys)
  encodeFile (kmeansFile params) kmeansModel
