import           Application.ETH80.PathGenerator
import           Application.Leaf.ArgsParser     as AP
import           Application.Leaf.Conduit
import           Classifier.LibLinear
import           Control.Monad                   as M
import           Control.Monad.Trans.Resource
import           CV.Array.LabeledArray
import           CV.Filter.GaussianFilter
import           CV.Filter.MorletWavelet
import           CV.Filter.PinwheelWavelet
import           CV.IO.ImageIO
import           CV.Statistics.KMeans
import           CV.Utility.FFT
import           CV.Utility.Parallel             as Par
import           Data.Binary
import           Data.Conduit
import           Data.Conduit.Binary             as CB
import           Data.Conduit.List               as CL
import           Data.List                       as L
import           Data.Vector                     as V
import           Data.Vector.Unboxed             as VU
import           System.Environment


main = do
  args <- getArgs
  params <- parseArgs args
  filterParams <-
    fmap
      (\x -> read x :: PinwheelWaveletParams -- MorletWaveletParams -- PinwheelWaveletParams -- PolarSeparableFilterParamsGrid -- FourierMellinTransformParamsGrid
       ) .
    readFile $
    (paramsFileName params)
  kmeansModel <- decodeFile (kmeansFile params)
  let parallelParams =
        ParallelParams
        { Par.numThread = AP.numThread params
        , Par.batchSize = AP.batchSize params
        }
      gFilterParams =
        GaussianFilterParams
          (gaussianScale params)
          (imageSize params)
          (imageSize params)
      fftwWisdom = FFTWWisdomPath (fftwWisdomPath params)
  labelPath <- pathGenerator (inputFile params)
  fftw <- initializefftw fftwWisdom
  filters <- makeFilterConvolution fftw filterParams Normal :: IO PinwheelWaveletConvolution -- MorletWaveletConvolution -- PinwheelWaveletConvolution --  PolarSeparableFilterGridConvolution -- FourierMellinTransformConvolution
  gFilters <- makeFilterConvolution fftw gFilterParams Normal :: IO GaussianFilterConvolution
  features <-
    M.mapM
      (\x ->
          let (labels, paths) = L.unzip x
          in runResourceT $
             sourceList paths $$ readImageConduit True =$=
             mergeSource (sourceList labels) =$=
             filterConduit'
               parallelParams
               fftw
               [filters]
               gFilters
               False
               (stride params) =$=
             kmeansConduit parallelParams kmeansModel =$=
             featurePtrConduit =$=
             CL.consume)
      labelPath
  zs <-
    M.mapM
      (\i ->
          let (xs, (y:ys)) = L.splitAt i features
              trainingSet = L.concat $ xs L.++ ys
              testingSet = y
              trainParams =
                TrainParams
                { trainSolver = L2R_L2LOSS_SVC_DUAL
                , trainC = (c params)
                , trainNumExamples = L.length trainingSet
                , trainFeatureIndexMax =
                  V.length (center kmeansModel) *
                  VU.length (V.head . center $ kmeansModel)
                , trainModel = (modelName params)
                }
          in trainNPredict trainParams trainingSet testingSet)
      [0 .. (L.length features - 1)]
  let (numCorrect, numTotal) = L.foldl1' (\(a, b) (c, d) -> (a + c, b + d)) zs
  print (fromIntegral numCorrect / fromIntegral numTotal)
