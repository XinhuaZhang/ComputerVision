import           Application.ECCV2018.ArgsParser          as AP
import           Application.ECCV2018.Conduit
import           Application.ECCV2018.Convolution.Conduit
import           Application.ECCV2018.Utility
import           Control.Arrow
import           Control.Monad                            as M
import           Control.Monad.Trans.Resource
import           CV.Array.Image
import           CV.Array.LabeledArray
import           CV.Filter.PolarSeparableFilter
import           CV.Utility.Parallel                      as Par
import           Data.Array.Repa
import           Data.Binary
import           Data.Complex
import           Data.Conduit
import           Data.Conduit.Binary                      as CB
import           Data.Conduit.List                        as CL
import           Data.List                                as L
import           Data.Vector.Unboxed                      as VU
import           System.Environment

main = do
  args <- getArgs
  params <- parseArgs args
  print params
  ((LabeledArray _ img):_) <-
    runResourceT $
    CB.sourceFile (inputFile params) $$ readLabeledImagebinaryConduit =$=
    CL.take 1
  let (Z :. _ :. rows :. cols) = extent img
      parallelParams =
        ParallelParams
        { Par.numThread = AP.numThread params
        , Par.batchSize = AP.batchSize params
        }
      filterParamsList = L.map (filterParamsFunc rows cols) (filterType params)
      invariantScatteringFilterParamsList =
        L.map
          (invariantScatteringFilterParamsFunc rows cols)
          (filterType params)
  M.mapM_ print filterParamsList
  M.mapM_ print invariantScatteringFilterParamsList
  (_plan, filters) <-
    makePolarSeparableFilterConvolutionList getEmptyPlan filterParamsList
  (plan, invariantScatteringFilters) <-
    makePolarSeparableFilterConvolutionList
      _plan
      invariantScatteringFilterParamsList
  writeFile (paramsFileName params) . show $
    [filterParamsList, invariantScatteringFilterParamsList]
  runResourceT $
    CB.sourceFile (inputFile params) $$ readLabeledImagebinaryConduit =$=
    (if variedSizeImageFlag params
       then polarSeparableFilterConvolutionConduitVariedSize
              parallelParams
              plan
              filters
              invariantScatteringFilters
              (numScatteringLayer params)
       else polarSeparableFilterConvolutionConduit
              parallelParams
              plan
              filters
              invariantScatteringFilters
              (numScatteringLayer params)) =$=
    invariantFeatureExtractionConduit parallelParams (stride params) =$=
    kmeansSink
      parallelParams
      (numGMMExample params)
      (numGaussian params)
      (kmeansFile params)
      (threshold params)
