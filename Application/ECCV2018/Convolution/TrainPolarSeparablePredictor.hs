import           Application.ECCV2018.ArgsParser              as AP
import           Application.ECCV2018.Conduit
import           Application.ECCV2018.Convolution.Conduit
import           Application.ECCV2018.ObjectPredictor.Conduit
import           Application.ECCV2018.Utility
import           Classifier.LibLinear
import           Control.Monad                                as M
import           Control.Monad.Trans.Resource
import           CV.Array.LabeledArray
import           CV.Filter.PolarSeparableFilter
import           CV.Utility.Parallel                          as Par
import           Data.Array.Repa
import           Data.Binary
import           Data.Complex
import           Data.Conduit
import           Data.Conduit.Binary                          as CB
import           Data.Conduit.List                            as CL
import           Data.List                                    as L
import           Data.Vector.Unboxed                          as VU
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
      filterParamsList = L.map (filterParamsFunc rows cols (alpha params)) (filterType params)
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
  (xsList:_) <-
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
    CL.map snd =$=
    -- mergeSource
    --   (CB.sourceFile (inputFile params) =$= readLabeledImagebinaryConduit =$=
    --    CL.map (\(LabeledArray _ arr) -> arr)) =$=
    -- splitObjectConduit parallelParams (stride params) (threshold params) =$=
    splitOriginsConduit parallelParams (centerLength params) (stride params) =$=
    CL.take 1
  featurePtrs <-
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
    CL.map snd =$=
    -- mergeSource
    --   (CB.sourceFile (inputFile params) =$= readLabeledImagebinaryConduit =$=
    --    CL.map (\(LabeledArray _ arr) -> arr)) =$=
    -- splitObjectConduit parallelParams (stride params) (threshold params) =$=
    splitOriginsConduit parallelParams (centerLength params) (stride params) =$=
    ojbectFeaturePtrConduit =$=
    CL.take (numGMMExample params)
  M.sequence_ $
    L.zipWith3
      (\i featurePtrLayerList xs ->
         M.sequence_ $
         L.zipWith3
           (\j featurePtr x -> do
              let trainParams =
                    TrainParams
                    { trainSolver = L2R_L2LOSS_SVC_DUAL
                    , trainC = c params
                    , trainNumExamples = L.length featurePtr
                    , trainFeatureIndexMax = VU.length . snd . L.head $ x
                    , trainModel =
                        originModelName params L.++ "_" L.++ show i L.++ "_" L.++
                        show j
                    }
                  (labels, features) = L.unzip featurePtr
              print trainParams
              train trainParams labels features)
           [1 ..]
           featurePtrLayerList
           xs)
      [1 ..]
      (L.map (L.map L.concat . L.transpose) . L.transpose $ featurePtrs)
      xsList
