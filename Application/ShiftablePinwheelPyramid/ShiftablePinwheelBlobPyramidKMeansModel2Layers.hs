import           Application.ShiftablePinwheelPyramid.ArgsParser as AP
import           Application.ShiftablePinwheelPyramid.Conduit
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
        ShiftablePinwheelBlobPyramidParams
        { shiftablePinwheelBlobPyramidNumLayers = 3
        , shiftablePinwheelBlobPyramidNumCenters = L.length centers
        , shiftablePinwheelBlobPyramidNumChannels = 3
        , shiftablePinwheelBlobPyramidNumTheta = 128
        , shiftablePinwheelBlobPyramidNumLogR = 128
        , shiftablePinwheelBlobPyramidK = 12
        }
      filters = generateShiftablePinwheelBlobPyramidFilters2Layers filterParams
      centers =
        [ (i, j)
        | i <- generateCenters (imageSize params) (numGrid params)
        , j <- generateCenters (imageSize params) (numGrid params)
        ]
  writeFile (paramsFileName params) . show $ filterParams
  fftw <- initializefftw FFTWWisdomNull
  xs <-
    runResourceT $
    CB.sourceFile (inputFile params) $$ readLabeledImagebinaryConduit =$=
    logpolarImageConduit
      parallelParams
      (shiftablePinwheelBlobPyramidNumTheta filterParams)
      (shiftablePinwheelBlobPyramidNumLogR filterParams)
      centers
      (radius params)
      (logpolarFlag params) =$=
    shiftablePinwheelBlobPyramidConduit2Layers fftw (stride params) filters =$=
    CL.take (numGMMExample params)
  let (ls, ys) = L.unzip xs
  kmeansModel <-
    M.mapM
      (kmeans parallelParams (numGaussian params) (kmeansFile params) 0.005)
      (L.map L.concat . L.transpose $ ys)
  encodeFile (kmeansFile params) kmeansModel
