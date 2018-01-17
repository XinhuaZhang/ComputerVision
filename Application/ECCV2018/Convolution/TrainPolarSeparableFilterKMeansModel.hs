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
  print filterParamsList
  (plan, filters) <-
    makePolarSeparableFilterConvolutionList getEmptyPlan filterParamsList
  writeFile (paramsFileName params) . show $ filterParamsList
  runResourceT $
    CB.sourceFile (inputFile params) $$ readLabeledImagebinaryConduit =$=
    polarSeparableFilterConvolutionConduit parallelParams plan filters =$=
    invariantFeatureExtractionConduit parallelParams (stride params) =$=
    kmeansSink
      parallelParams
      (numGMMExample params)
      (numGaussian params)
      (kmeansFile params)
      (threshold params)
  undefined
