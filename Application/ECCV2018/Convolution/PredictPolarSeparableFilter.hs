import           Application.ECCV2018.ArgsParser          as AP
import           Application.ECCV2018.Conduit
import           Application.ECCV2018.Convolution.Conduit
import           Classifier.LibLinear
import           Control.Monad                            as M
import           Control.Monad.Trans.Resource
import           CV.Array.LabeledArray
import           CV.Filter.PolarSeparableFilter
import           CV.Utility.Parallel                      as Par
import           Data.Array.Repa
import           Data.Binary
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
  let parallelParams =
        ParallelParams
        { Par.numThread = AP.numThread params
        , Par.batchSize = AP.batchSize params
        }
  kmeansModel <- decodeFile (kmeansFile params)
  (filterParamsList:invariantScatteringFilterParamsList:_) <-
    fmap (\x -> read x :: [[PolarSeparableFilterParams]]) . readFile $
    (paramsFileName params)
  (_plan, filters) <-
    makePolarSeparableFilterConvolutionList getEmptyPlan filterParamsList
  (plan, invariantScatteringFilters) <-
    makePolarSeparableFilterConvolutionList
      _plan
      invariantScatteringFilterParamsList
  if pcaFlag params
    then do
      pcaModel <- decodeFile (pcaFile params)
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
        kmeansConduit parallelParams kmeansModel =$=
        pcaConduit parallelParams pcaModel =$=
        featureConduit =$=
        predict (modelName params) ((modelName params) L.++ ".out")
    else runResourceT $
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
         kmeansConduit parallelParams kmeansModel =$=
         featureConduit =$=
         predict (modelName params) ((modelName params) L.++ ".out")
