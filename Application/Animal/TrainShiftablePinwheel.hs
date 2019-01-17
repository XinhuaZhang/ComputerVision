import           Application.ShiftablePinwheelPyramid.ArgsParser as AP
import           Application.ShiftablePinwheelPyramid.Conduit
import           Classifier.LibLinear
import           Control.Arrow
import           Control.Monad                                   as M
import           Control.Monad.Trans.Resource
import           CV.Array.LabeledArray
import           CV.Filter.ShiftablePinwheelPyramid
import           CV.IO.ImageIO
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
      filterParams =
        ShiftablePinwheelPyramidParams
        { shiftablePinwheelPyramidNumLayers = 3
        , shiftablePinwheelPyramidNumCenters = L.length centers
        , shiftablePinwheelPyramidNumChannels = 3
        , shiftablePinwheelPyramidNumTheta = 512
        , shiftablePinwheelPyramidNumLogR = 128
        }
  writeFile (paramsFileName params) . show $ filterParams
  fftw <- initializefftw FFTWWisdomNull
  (x:_) <-
    runResourceT $
    imagePathSource (inputFile params) $$ readImageConduit False =$=
    logpolarImageConduit1
      parallelParams
      (shiftablePinwheelPyramidNumTheta filterParams)
      (shiftablePinwheelPyramidNumLogR filterParams)
      centers
      (radius params)
      (logpolarFlag params) =$=
    mergeSource (labelSource (labelFile params)) =$=
    shiftablePinwheelConduit fftw (stride params) =$=
    CL.map (second $ VU.concat) =$=
    CL.take 1
  featurePtr <-
    runResourceT $
    imagePathSource (inputFile params) $$ readImageConduit False =$=
    logpolarImageConduit1
      parallelParams
      (shiftablePinwheelPyramidNumTheta filterParams)
      (shiftablePinwheelPyramidNumLogR filterParams)
      centers
      (radius params)
      (logpolarFlag params) =$=
    mergeSource (labelSource (labelFile params)) =$=
    shiftablePinwheelConduit fftw (stride params) =$=
    CL.map (second $ VU.concat) =$=
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
