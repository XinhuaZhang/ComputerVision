import           Application.ShiftablePinwheelPyramid.ArgsParser as AP
import           Application.ShiftablePinwheelPyramid.Conduit
import           Control.Monad                                   as M
import           Control.Monad.Trans.Resource
import           CV.Array.LabeledArray
import           CV.Filter.ShiftablePinwheelPyramid
import           CV.Statistics.KMeans
import           CV.Statistics.PCA
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
        ShiftablePinwheelPyramidParams
        { shiftablePinwheelPyramidNumLayers = 1
        , shiftablePinwheelPyramidNumCenters = L.length centers
        , shiftablePinwheelPyramidNumChannels = 3
        , shiftablePinwheelPyramidNumTheta = 128
        , shiftablePinwheelPyramidNumLogR = 128
        }
      filters = generateShiftablePinwheelRingPyramidFilters filterParams
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
      (shiftablePinwheelPyramidNumTheta filterParams)
      (shiftablePinwheelPyramidNumLogR filterParams)
      centers
      (radius params)
      (logpolarFlag params) =$=
    shiftablePinwheelRingPyramidConduit fftw filters =$=
    CL.take (numGMMExample params)
  let (ls, ys) = L.unzip xs
  --     (pcaMat, _, zs) =
  --       L.unzip3 .
  --       L.map (pcaSVD parallelParams (numPrincipal params)) .
  --       L.map L.concat . L.transpose $
  --       ys
  -- encodeFile (pcaFile params) pcaMat
  kmeansModel <-
    M.mapM
      (kmeans
         parallelParams
         (numGaussian params)
         (kmeansFile params)
         (threshold params))
      (L.map L.concat . L.transpose $ ys)
      -- zs
  encodeFile (kmeansFile params) kmeansModel