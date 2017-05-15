import           Application.Leaf.ArgsParser         as AP
import           Application.Leaf.Conduit
import           Application.MultiDimensionalGMM.GMM
import           Application.RecenterImage.Conduit
import           Control.Monad                       as M
import           Control.Monad.Trans.Resource
import           CV.Array.LabeledArray
import           CV.Utility.Parallel                 as Par
import           CV.V4Filter
import           CV.V4FilterConvolution
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
  let parallelParams =
        ParallelParams
        { Par.numThread = AP.numThread params
        , Par.batchSize = AP.batchSize params
        }
      m = 30
      filterParams =
        V4SeparableFilterParamsAxis
        { v4SeparableFilterParamsAxisSeparableFilterRows = imageSize params
        , v4SeparableFilterParamsAxisSeparableFilterCols = imageSize params
        , v4SeparableFilterParamsAxisPolarSeparablePolarFactor = 1
        , v4SeparableFilterParamsAxisPolarSeparableScale = [56]
        , v4SeparableFilterParamsAxisPolarSeparableFreq = [-4 .. 4]
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
  filtersF <-
    M.mapM (fourierTransformFilter (imageSize params, imageSize params)) filters
  writeFile (paramsFileName params) . show $ filterParams
  (_, vecs) <-
    runResourceT $
    CB.sourceFile (inputFile params) $$ readLabeledImagebinaryConduit =$=
    applyV4SeparableFilterConvolutionLabeledArrayConduit filtersF =$=
    orientationHistogramConduit
      parallelParams
      (patchSize params)
      (stride params)
      (numBin params) =$=
    pcaSink parallelParams (pcaFile params) (numPrincipal params)
  withBinaryFile (gmmFile params) WriteMode $
    \h ->
       runResourceT $
       CL.sourceList vecs $$
       hGMMSink2
         parallelParams
         h
         (numGaussian params)
         (threshold params)
         (numGMMExample params)
