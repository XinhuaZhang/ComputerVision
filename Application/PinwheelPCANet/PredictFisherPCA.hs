import           Application.PinwheelPCANet.ArgsParser     as Parser
import           Application.MultiDimensionalGMM.FisherKernel
import           Application.MultiDimensionalGMM.GMM
import           Application.MultiDimensionalGMM.MixtureModel
import           Application.PinwheelPCANet.PCA
import           Classifier.LibLinear
import           Control.Arrow
import           Control.Monad
import           Control.Monad.Trans.Resource
import           CV.Array.LabeledArray
import           CV.Feature.PolarSeparable
import           CV.Filter.PolarSeparableFilter
import           CV.Utility.Parallel            as Parallel
import           Data.Array.Repa                as R
import           Data.Conduit
import           Data.Conduit.Binary            as CB
import           Data.Conduit.List              as CL
import           Data.List                      as L
import           Data.Set                       as S
import           Data.Vector.Unboxed            as VU
import           Prelude                        as P
import           System.Environment
import CV.Filter.GaussianFilter

main = do
  args <- getArgs
  if P.null args
    then error "run with --help to see options."
    else return ()
  params <- parseArgs args
  gmm <- readGMM (gmmFile params) :: IO [GMM]
  pcaMatrixes <- readMatrixes (pcaFile params)
  imageSize <-
    if isFixedSize params
      then do
        xs <-
          runResourceT $
          sourceFile (inputFile params) $$ readLabeledImagebinaryConduit =$=
          CL.take 1
        let (LabeledArray _ arr) = L.head xs
            (Z :. _ :. ny :. nx) = extent arr
        return (ny, nx)
      else return (0, 0)
  let parallelParams =
        ParallelParams
        { Parallel.numThread = Parser.numThread params
        , Parallel.batchSize = Parser.batchSize params
        }
      filterParamsSetFunc fa freq' =
        PolarSeparableFilterParamsSet
        { getSizeSet = imageSize
        , getDownsampleFactorSet = fa
        , getScaleSet = S.fromDistinctAscList (scale params)
        , getRadialFreqSet = S.fromDistinctAscList [0 .. (freq' - 1)]
        , getAngularFreqSet = S.fromDistinctAscList [0 .. (freq'  - 1)]
        , getNameSet = Pinwheels
        }
      filterParamsList = L.zipWith filterParamsSetFunc [1,2,2,2,1] (freq params)
      gaussianFilterParamsList =
        L.map
          (\gScale -> GaussianFilterParams gScale imageSize)
          (gaussianScale params)
      magnitudeConduit =
        pinwheelPCANetVariedSizeConduit
          parallelParams
          filterParamsList
          (downsampleFactor params)
          pcaMatrixes
      -- magnitudeConduit =
      --   if isFixedSize params
      --     then multiLayerMagnitudeFixedSizedConduit
      --            parallelParams
      --            (L.map makeFilterSet filterParamsList)
      --            (downsampleFactor params)
      --     else multiLayerMagnitudeVariedSizedConduit
      --            parallelParams
      --            filterParamsList
      --            (downsampleFactor params)
  print params
  runResourceT $
    sourceFile (inputFile params) $$ readLabeledImagebinaryConduit =$= magnitudeConduit =$=
    (fisherVectorConduit1 parallelParams gmm) =$=
    CL.map (fromIntegral *** (getFeature . Dense . VU.toList)) =$=
    predict (modelName params) ((modelName params) P.++ ".out")
