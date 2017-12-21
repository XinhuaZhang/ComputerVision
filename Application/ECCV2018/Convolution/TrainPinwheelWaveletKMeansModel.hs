import           Application.ECCV2018.ArgsParser          as AP
import           Application.ECCV2018.Conduit
import           Application.ECCV2018.Convolution.Conduit
import           Control.Arrow
import           Control.Monad                            as M
import           Control.Monad.Trans.Resource
import           CV.Array.Image
import           CV.Array.LabeledArray
import           CV.Filter.PinwheelWavelet
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
      filterParams =
        PinwheelWaveletParams
        { pinwheelWaveletRows = rows
        , pinwheelWaveletCols = cols
        , pinwheelWaveletGaussianScale = 0.1 * pi
        , pinwheelWaveletScale = L.map (\x -> 2 ** (x / 8)) [0 .. 0]
        , pinwheelWaveletRadialFreqs = L.map (\x -> x / 8 * pi) [0, 6, 8]
        , pinwheelWaveletAngularFreqs = [-7 .. 7]
        , pinwheelWaveletRadius = [3, 5, 7, 9, 11]
        }
  (plan, filters) <-
    makeFilterConvolution getEmptyPlan filterParams Normal :: IO (DFTPlan, PinwheelWaveletConvolution)
  writeFile (paramsFileName params) . show $ filterParams
  runResourceT $
    CB.sourceFile (inputFile params) $$ readLabeledImagebinaryConduit =$=
    pinwheelWaveletConvolutionConduit parallelParams plan filters =$=
    listConduit =$=
    trainFeatureConduit parallelParams (stride params) =$=
    kmeansSink
      parallelParams
      (numGMMExample params)
      (numGaussian params)
      (kmeansFile params)
      (threshold params)
