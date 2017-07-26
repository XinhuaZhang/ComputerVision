import           Application.CIFAR10.ArgsParser as AP
import           Application.Leaf.Conduit
import           Classifier.LibLinear
import           Control.Monad                  as M
import           Control.Monad.Trans.Resource
import           CV.Array.LabeledArray
import           CV.Filter.GaussianFilter
--import           CV.Filter.PinwheelWavelet
import           CV.Filter.MorletWavelet
import           CV.IO.ImageIO
import           CV.Statistics.KMeans
import           CV.Utility.FFT
import           CV.Utility.Parallel            as Par
import           Data.Array.Repa
import           Data.Binary
import           Data.Complex
import           Data.Conduit
import           Data.Conduit.Binary            as CB
import           Data.Conduit.List              as CL
import           Data.List                      as L
import           Data.Vector.Unboxed            as VU
import           System.Environment
import CV.Utility.RepaArrayUtility



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
      -- filterParams =
      --   PinwheelWaveletParams
      --   { pinwheelWaveletRows = imageSize params
      --   , pinwheelWaveletCols = imageSize params
      --   , pinwheelWaveletGaussianScale = 0.1
      --   , pinwheelWaveletScale = L.map (\x -> sqrt 2 ** x) [0 .. 2]
      --   , pinwheelWaveletRadialScale = L.map (\x -> (1 / sqrt 2) ** x) [0 .. 0]
      --   , pinwheelWaveletRadialFreqs = 3 / 4 * pi
      --   , pinwheelWaveletAngularFreqs = [0 .. 15]
      --   , pinwheelWaveletRadius = [0 .. 4]
      --   }
      m = 18
      filterParams =
        MorletWaveletParams
        { morletWaveletRows = imageSize params
        , morletWaveletCols = imageSize params
        , morletWaveletFreq = 3 * pi / 4
        , morletWaveletGaussianScale = 0.85 --0.25 * pi
        , morletWaveletOrientation = [m .. 180 - m]
        , morletWaveletScale = L.map (\x -> 2 ** (x / 2)) [0 .. 7]
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
    imagePathSource (inputFile params) $$ readImageConduit True =$=
    CL.map
      (VU.map (/ 3) . L.foldl1' (VU.zipWith (+)) . arrayToUnboxed . imageContent) =$=
    CL.take 1
  generateWisdom
    fftwInit
    (fftwWisdomPath params)
    (imageSize params)
    (imageSize params) .
    VU.convert . VU.map (:+ 0) . L.head $
    imgs
  fftw <- initializefftw fftwWisdom
  filters <-
    makeFilterConvolution fftw filterParams Normal :: IO MorletWaveletConvolution -- PinwheelWaveletConvolution
  gFilters <-
    makeFilterConvolution fftw gFilterParams Normal :: IO GaussianFilterConvolution
  xs <-
    runResourceT $
    imagePathSource (inputFile params) $$ readImageConduit True =$=
    mergeSource (labelSource (labelFile params)) =$=
    filterConduit' parallelParams fftw [filters] gFilters False (stride params) =$=
    CL.take (numGMMExample params)
  let (ls, ys) = L.unzip xs
  kmeansModel <- kmeans parallelParams (numGaussian params) 0.005 (L.concat ys)
  encodeFile (kmeansFile params) kmeansModel
