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
      filterParams =
        ShiftablePinwheelPyramidParams
        { shiftablePinwheelPyramidNumLayers = 1
        , shiftablePinwheelPyramidNumCenters = L.length centers
        , shiftablePinwheelPyramidNumChannels = 3
        , shiftablePinwheelPyramidNumTheta = 512
        , shiftablePinwheelPyramidNumLogR = 512
        }
      filters = generateShiftablePinwheelRingPyramidFilters filterParams
      centers =
        [ (i, j)
        | i <- generateCenters (imageSize params) (numGrid params)
        , j <- generateCenters (imageSize params) (numGrid params) ]
  writeFile (paramsFileName params) . show $ filterParams
  fftw <- initializefftw FFTWWisdomNull
  xs <-
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
    CL.take (numGMMExample params)
  let (ls, ys) = L.unzip xs
  kmeansModel <-
    M.mapM
      (kmeans parallelParams (numGaussian params) (kmeansFile params) 0.005)
      (L.map L.concat . L.transpose $ ys)
  encodeFile (kmeansFile params) kmeansModel
