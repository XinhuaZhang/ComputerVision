import           Application.ECCV2018.ArgsParser              as AP
import           Application.ECCV2018.Conduit
import           Application.ECCV2018.Convolution.Conduit
import           Application.ECCV2018.ObjectPredictor.Conduit
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
      gaussianPinwheelParams =
        GaussianPinwheelParams
        { getGaussianPinwheelRows = rows
        , getGaussianPinwheelCols = cols
        , getGaussianPinwheelScale = L.map (* pi) [0.25]
        , getGaussianPinwheelRadialFreq = [0 .. 7]
        , getGaussianPinwheelAngularFreq = [0 .. 7]
        }
      pinwheelRingParams =
        PinwheelRingParams
        { pinwheelRingRows = rows
        , pinwheelRingCols = cols
        , pinwheelRingGaussianScale = 0.1 * pi
        , pinwheelRingScale = L.map (\x -> 2 ** (x / 4)) [0 .. 0]
        , pinwheelRingRadialFreqs = [0 .. 7] -- L.map (\x -> x / 8 * pi) [0, 6, 8, 10]
        , pinwheelRingAngularFreqs = [0 .. 7]
        , pinwheelRingRadius = [4, 5, 6]
        }
      filterParamsList = [gaussianPinwheelParams, pinwheelRingParams]
  print filterParamsList
  (plan, filters) <-
    makePolarSeparableFilterConvolutionList getEmptyPlan filterParamsList
  writeFile (originPredictorParamsFileName params) . show $ filterParamsList
  (xs:_) <-
    runResourceT $
    CB.sourceFile (inputFile params) $$ readLabeledImagebinaryConduit =$=
    polarSeparableFilterConvolutionConduit parallelParams plan filters =$=
    CL.map (L.map (L.map (\(LabeledArray _ arr) -> arr))) =$=
    mergeSource
      (CB.sourceFile (inputFile params) =$= readLabeledImagebinaryConduit =$=
       CL.map (\(LabeledArray _ arr) -> arr)) =$=
    splitObjectConduit parallelParams (stride params) (threshold params) =$=
    CL.take 1
  featurePtrs <-
    runResourceT $
    CB.sourceFile (inputFile params) $$ readLabeledImagebinaryConduit =$=
    polarSeparableFilterConvolutionConduit parallelParams plan filters =$=
    CL.map (L.map (L.map (\(LabeledArray _ arr) -> arr))) =$=
    mergeSource
      (CB.sourceFile (inputFile params) =$= readLabeledImagebinaryConduit =$=
       CL.map (\(LabeledArray _ arr) -> arr)) =$=
    splitObjectConduit parallelParams (stride params) (threshold params) =$=
    ojbectFeaturePtrConduit =$=
    CL.take (numGMMExample params)
  M.sequence_ $
    L.zipWith3
      (\i featurePtr x -> do
         let trainParams =
               TrainParams
               { trainSolver = L2R_L2LOSS_SVC_DUAL
               , trainC = c params
               , trainNumExamples = L.length featurePtr
               , trainFeatureIndexMax = VU.length . snd . L.head $ x
               , trainModel = originModelName params L.++ "_" L.++ show i
               }
             (labels, features) = L.unzip featurePtr
         print trainParams
         train trainParams labels features)
      [1 ..]
      (L.map L.concat . L.transpose $ featurePtrs)
      xs
