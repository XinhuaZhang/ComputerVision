import           Application.Leaf.ArgsParser                  as AP
import           Application.Leaf.Conduit
import           Application.RecenterImage.Conduit
import           Classifier.LibLinear
import           Control.Concurrent.MVar                      (newMVar)
import           Control.Monad                                as M
import           Control.Monad.Trans.Resource
import           CV.Array.LabeledArray
import           CV.Filter.GaussianFilter                     as Gaussian
import           CV.Utility.Parallel                          as Par
import           CV.V4Filter
import           CV.V4FilterConvolution
import           Data.Array.Repa                              as R
import           Data.Binary
import           Data.Conduit
import           Data.Conduit.Binary                          as CB
import           Data.Conduit.List                            as CL
import           Data.List                                    as L
import           Data.Vector                                  as V
import           Data.Vector.Unboxed                          as VU
import           System.Environment
import           System.IO


main = do
  args <- getArgs
  params <- parseArgs args
  print params
  filterParams <-
    fmap (\x -> read x :: V4SeparableFilterParamsAxis) . readFile $
    ("Expansion_" L.++ paramsFileName params)
  convolutionFilterParams <-
    fmap (\x -> read x :: V4SeparableFilterParamsAxisConvolution) . readFile $
    ("Convolution_" L.++ paramsFileName params)
  let parallelParams =
        ParallelParams
        { Par.numThread = AP.numThread params
        , Par.batchSize = AP.batchSize params
        }
      convolutionGFilterParams =
        L.map
          (\s -> GaussianFilterParams s (imageSize params) (imageSize params)) .
        gaussianScale $
        params
      convolutionFilters = generateV4SeparableFilterAxisConvolution convolutionFilterParams
      gaussianFilterParams =
        GaussianFilterParams 32 (imageSize params) (imageSize params)
  lock <- newMVar ()
  convolutionFiltersF <-
    M.mapM (fourierTransformFilter lock (imageSize params, imageSize params)) convolutionFilters
  gConvolutionFilters <-
    M.mapM (fmap (getFilter . fmap toUnboxed) . Gaussian.makeFilter lock) convolutionGFilterParams
  gaussianFilter <- Gaussian.makeFilter lock gaussianFilterParams
  runResourceT $
    CB.sourceFile (inputFile params) $$ readLabeledImagebinaryConduit =$=
    applyV4SeparableFilterConvolutionLabeledArrayConduit
      lock
      parallelParams
      (stride params)
      gConvolutionFilters
      convolutionFiltersF =$=
    applyV4SeparableFilterLabeledArrayWithCenterConduit1
      lock
      parallelParams
      gaussianFilter
      filterParams =$=
    featureConduit =$=
    predict (modelName params) ((modelName params) L.++ ".out")
