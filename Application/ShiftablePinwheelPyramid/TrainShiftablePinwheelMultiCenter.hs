import           Application.ShiftablePinwheelPyramid.ArgsParser as AP
import           Application.ShiftablePinwheelPyramid.Conduit
import           Classifier.LibLinear
import           Control.Monad                                   as M
import           Control.Monad.Trans.Resource
import           CV.Array.LabeledArray
import           CV.Filter.ShiftablePinwheelPyramid
import           CV.Statistics.KMeans
import           CV.Utility.FFT
import           CV.Utility.Parallel                             as Par
import           Data.Binary
import           Data.Conduit
import           Data.Conduit.Binary                             as CB
import           Data.Conduit.List                               as CL
import           Data.List                                       as L
import           Data.Vector                                     as V
import           Data.Vector.Unboxed                             as VU
import           System.Environment

main = do
  args <- getArgs
  params <- parseArgs args
  print params
  filterParams <-
    fmap (\x -> read x :: ShiftablePinwheelPyramidParams) . readFile $
    (paramsFileName params)
  kmeansModel <- decodeFile (kmeansFile params)
  -- pcaMat <- decodeFile (pcaFile params)
  let parallelParams =
        ParallelParams
        { Par.numThread = AP.numThread params
        , Par.batchSize = AP.batchSize params
        }
      centers =
        [ (i, j)
        | i <- generateCenters (imageSize params) (numGrid params)
        , j <- generateCenters (imageSize params) (numGrid params)
        ]
  fftw <- initializefftw FFTWWisdomNull
  (x:_) <-
    runResourceT $
    CB.sourceFile (inputFile params) $$ readLabeledImagebinaryConduit =$=
    logpolarImageConduit
      parallelParams
      (shiftablePinwheelPyramidNumTheta filterParams)
      (shiftablePinwheelPyramidNumLogR filterParams)
      centers
      (radius params)
      (logpolarFlag params) =$=
    shiftablePinwheelConduit fftw (stride params) =$=
    -- pcaConduit1 parallelParams pcaMat =$=
    kmeansConduit1 parallelParams kmeansModel =$=
    CL.take 1
  featurePtr <-
    runResourceT $
    CB.sourceFile (inputFile params) $$ readLabeledImagebinaryConduit =$=
    logpolarImageConduit
      parallelParams
      (shiftablePinwheelPyramidNumTheta filterParams)
      (shiftablePinwheelPyramidNumLogR filterParams)
      centers
      (radius params)
      (logpolarFlag params) =$=
    shiftablePinwheelConduit fftw (stride params) =$=
    -- pcaConduit1 parallelParams pcaMat =$=
    kmeansConduit1 parallelParams kmeansModel =$=
    featurePtrConduit =$=
    CL.consume
  let trainParams =
        TrainParams
        { trainSolver = L2R_L2LOSS_SVC_DUAL
        , trainC = (c params)
        , trainNumExamples = L.length featurePtr
        , trainFeatureIndexMax = VU.length . snd $ x
        , trainModel = (modelName params)
        }
      (labels, features) = L.unzip featurePtr
  print trainParams
  train trainParams labels features
