import           Application.Leaf.ArgsParser      as AP
import           Application.Leaf.Conduit
import           Classifier.LibLinear
import           Control.Monad                    as M
import           Control.Monad.Trans.Resource
import           CV.Array.LabeledArray
--import           CV.Filter.FourierMellinTransform
--import           CV.Filter.PolarSeparableFilter
import           CV.Filter.PinwheelWavelet
import           CV.Filter.GaussianFilter
import           CV.Statistics.KMeans
import           CV.Utility.FFT
import           CV.Utility.Parallel              as Par
import           Data.Array.Repa
import           Data.Binary
import           Data.Complex
import           Data.Conduit
import           Data.Conduit.Binary              as CB
import           Data.Conduit.List                as CL
import           Data.List                        as L
import           Data.Vector.Unboxed              as VU
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
      n = 15
      filterParams =
        PinwheelWaveletParams
        { pinwheelWaveletRows = imageSize params
        , pinwheelWaveletCols = imageSize params
        , pinwheelWaveletGaussianScale = 0.5
        , pinwheelWaveletScale = L.map (\x -> sqrt 2 ** x) [0 .. 3]
        , pinwheelWaveletRadialScale = L.map (\x -> ((2 ** (-x / 2)))) [0 .. 3]
        , pinwheelWaveletRadialFreqs = 3 / 4 * pi
        , pinwheelWaveletAngularFreqs = [-7 .. 7]
        , pinwheelWaveletRadius = [1 .. 4]
        }
      gFilterParams =
        GaussianFilterParams
          (gaussianScale params)
          (imageSize params)
          (imageSize params)
      fftwWisdom = FFTWWisdomPath (fftwWisdomPath params)
  writeFile (paramsFileName params) . show $ filterParams
  fftwInit <- initializefftw FFTWWisdomNull
  imgs <-
    runResourceT $
    CB.sourceFile (inputFile params) $$ readLabeledImagebinaryConduit =$=
    CL.map (\(LabeledArray _ arr) -> toUnboxed arr) =$=
    CL.take 1
  generateWisdom
    fftwInit
    (fftwWisdomPath params)
    (imageSize params)
    (imageSize params) .
    VU.convert . VU.map (:+ 0) . L.head $
    imgs
  fftw <- initializefftw fftwWisdom
  filters <- makeFilterConvolution fftw filterParams Normal :: IO PinwheelWaveletConvolution
  gFilters <- makeFilterConvolution fftw gFilterParams Normal :: IO GaussianFilterConvolution
  xs <-
    runResourceT $
    CB.sourceFile (inputFile params) $$ readLabeledImagebinaryConduit =$=
    filterKeypointConduit parallelParams fftw filters 50 15 =$=
    CL.take (numGMMExample params)
  let (ls, ys) = L.unzip xs
  kmeansModel <-
    kmeans parallelParams (numGaussian params) (kmeansFile params) (L.concat ys)
  encodeFile (kmeansFile params) kmeansModel
