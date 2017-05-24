import           Application.Leaf.ArgsParser         as AP
import           Application.Leaf.Conduit
import           Application.MultiDimensionalGMM.GMM
import           Application.RecenterImage.Conduit
import           Control.Concurrent.MVar             (newMVar)
import           Control.Monad                       as M
import           Control.Monad.Trans.Resource
import           CV.Array.LabeledArray
import           CV.Filter.GaussianFilter            as Gaussian
import           CV.Utility.Parallel                 as Par
import           CV.V4Filter
import           CV.V4FilterConvolution
import           Data.Array.Repa                     as R
import           Data.Conduit
import           Data.Conduit.Binary                 as CB
import           Data.Conduit.List                   as CL
import           Data.List                           as L
import           Data.Vector.Unboxed                 as VU
import           System.Environment
import           System.IO

main = do
  args <- getArgs
  params <- parseArgs args
  print params
  let parallelParams =
        ParallelParams
        { Par.numThread = AP.numThread params
        , Par.batchSize = AP.batchSize params
        }
      m = 45
      filterParams =
        V4SeparableFilterParamsAxis
        { v4SeparableFilterParamsAxisSeparableFilterRows = imageSize params
        , v4SeparableFilterParamsAxisSeparableFilterCols = imageSize params
        , v4SeparableFilterParamsAxisPolarSeparablePolarFactor = 1
        , v4SeparableFilterParamsAxisPolarSeparableScale = [0]
        , v4SeparableFilterParamsAxisPolarSeparableFreq = [1 .. 4]
        , v4SeparableFilterParamsAxisPolarSeparableAngle = [0,m .. 90 - m]
        , v4SeparableFilterParamsAxisCartesianGratingScale =
          [ 2 ** (i / 2)
          | i <- [7 .. 10] ]
        , v4SeparableFilterParamsAxisCartesianGratingFreq = L.take 8 [1 .. 8]
        , v4SeparableFilterParamsAxisCartesianGratingAngle = [0,15 .. 360 - 15]
        , v4SeparableFilterParamsAxisHyperbolicSeparableScale =
          [ 2 ** (i / 2)
          | i <- [7 .. 10] ]
        , v4SeparableFilterParamsAxisHyperbolicSeparableUFreq = [0 .. 3]
        , v4SeparableFilterParamsAxisHyperbolicSeparableVFreq = [0 .. 3]
        , v4SeparableFilterParamsAxisHyperbolicSeparableAngle = 15
        , v4SeparableFilterParamsAxisSeparableFilterParams = P
        }
      filters = generateV4SeparableFilterAxis filterParams
      gFilterParams =
        L.map
          (\s -> GaussianFilterParams s (imageSize params) (imageSize params)) .
        gaussianScale $
        params
  lock <- newMVar ()
  filtersF <-
    M.mapM (fourierTransformFilter lock (imageSize params, imageSize params)) filters
  gFilters <-
    M.mapM (fmap (getFilter . fmap toUnboxed) . Gaussian.makeFilter lock) gFilterParams
  writeFile (paramsFileName params) . show $ filterParams
  (_, vecs) <-
    runResourceT $
    CB.sourceFile (inputFile params) $$ readLabeledImagebinaryConduit =$=
    applyV4SeparableFilterConvolutionLabeledArrayConduit lock parallelParams 1 gFilters filtersF =$=
    orientationHistogramConduit
      parallelParams
      (patchSize params)
      (stride params)
      (numBin params) =$=
    pcaSink
      parallelParams
      (pcaFile params)
      (numPrincipal params)
      (numGMMExample params)
  withBinaryFile (gmmFile params) WriteMode $
    \h ->
       runResourceT $
       CL.sourceList vecs $$
       hGMMSink1
         parallelParams
         h
         (numGaussian params)
         (threshold params)
         (numGMMExample params)
