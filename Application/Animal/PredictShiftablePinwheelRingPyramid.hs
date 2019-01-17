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
import           CV.IO.ImageIO

main = do
  args <- getArgs
  params <- parseArgs args
  print params
  filterParams <-
    fmap (\x -> read x :: ShiftablePinwheelPyramidParams) . readFile $
    (paramsFileName params)
  kmeansModels <- decodeFile (kmeansFile params)
  let parallelParams =
        ParallelParams
        { Par.numThread = AP.numThread params
        , Par.batchSize = AP.batchSize params
        }
      filters = generateShiftablePinwheelRingPyramidFilters filterParams
      centers =
        [ (i, j)
        | i <- generateCenters (imageSize params) (numGrid params)
        , j <- generateCenters (imageSize params) (numGrid params) ]
  fftw <- initializefftw FFTWWisdomNull
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
    shiftablePinwheelRingPyramidConduit parallelParams fftw filters =$=
    kmeansConduit parallelParams kmeansModels =$=
    featureConduit =$=
    predict (modelName params) ((modelName params) L.++ ".out")
