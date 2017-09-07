import           Application.CIFAR10.ArgsParser as AP
import           Application.Leaf.Conduit
import           Classifier.LibLinear
import           Control.Monad                  as M
import           Control.Monad.Trans.Resource
import           CV.Array.LabeledArray
import           CV.Filter.GaussianFilter
import           CV.Filter.PinwheelWavelet
--import           CV.Filter.MorletWavelet
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
      filterParams1 =
        PinwheelWaveletParams
        { pinwheelWaveletRows = imageSize params
        , pinwheelWaveletCols = imageSize params
        , pinwheelWaveletGaussianScale = 0.1 * pi
        , pinwheelWaveletScale = L.map (\x -> 2 ** (x / 1)) [0 .. 0]
        , pinwheelWaveletRadialScale = L.map (\x -> (1 / sqrt 2) ** x) [0 .. 0]
        , pinwheelWaveletRadialFreqs = L.map (\x -> x * pi) [0 .. 2]
        , pinwheelWaveletAngularFreqs = [0 .. 2]
        , pinwheelWaveletRadius = [1]
        }
      filterParams2 =
        PinwheelWaveletParams
        { pinwheelWaveletRows = imageSize params
        , pinwheelWaveletCols = imageSize params
        , pinwheelWaveletGaussianScale = 0.1 * pi
        , pinwheelWaveletScale = L.map (\x -> 2 ** (x / 1)) [0 .. 0]
        , pinwheelWaveletRadialScale = L.map (\x -> (1 / sqrt 2) ** x) [0 .. 0]
        , pinwheelWaveletRadialFreqs = L.map (\x -> x * pi) [0 .. 4]
        , pinwheelWaveletAngularFreqs = [0 .. 4]
        , pinwheelWaveletRadius = [2]
        }
      filterParams3 =
        PinwheelWaveletParams
        { pinwheelWaveletRows = imageSize params
        , pinwheelWaveletCols = imageSize params
        , pinwheelWaveletGaussianScale = 0.1 * pi
        , pinwheelWaveletScale = L.map (\x -> 2 ** (x / 1)) [0 .. 0]
        , pinwheelWaveletRadialScale = L.map (\x -> (1 / sqrt 2) ** x) [0 .. 0]
        , pinwheelWaveletRadialFreqs = L.map (\x -> x * pi) [0 .. 8]
        , pinwheelWaveletAngularFreqs = [0 .. 8]
        , pinwheelWaveletRadius = [3]
        }
      filterParams4 =
        PinwheelWaveletParams
        { pinwheelWaveletRows = imageSize params
        , pinwheelWaveletCols = imageSize params
        , pinwheelWaveletGaussianScale = 0.1 * pi
        , pinwheelWaveletScale = L.map (\x -> 2 ** (x / 1)) [0 .. 0]
        , pinwheelWaveletRadialScale = L.map (\x -> (1 / sqrt 2) ** x) [0 .. 0]
        , pinwheelWaveletRadialFreqs = L.map (\x -> x * pi) [0 .. 16]
        , pinwheelWaveletAngularFreqs = [0 .. 16]
        , pinwheelWaveletRadius = [4]
        }
      filterParamsList =
        [filterParams1, filterParams2, filterParams3, filterParams4]
      gFilterParams =
        GaussianFilterParams
          (gaussianScale params)
          (imageSize params)
          (imageSize params)
      fftwWisdom = FFTWWisdomPath (fftwWisdomPath params)
  writeFile (paramsFileName params) . show $ filterParamsList
  fftwInit <- initializefftw FFTWWisdomNull
  imgs <-
    runResourceT $
    imagePathSource (inputFile params) $$ readImageConduit False =$=
    CL.map (L.head . arrayToUnboxed . imageContent) =$=
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
    M.mapM (\filterParams -> makeFilterConvolution fftw filterParams Normal) filterParamsList :: IO [PinwheelWaveletConvolution] -- MorletWaveletConvolution -- PinwheelWaveletConvolution
  gFilters <- makeFilterConvolution fftw gFilterParams Normal :: IO GaussianFilterConvolution
  xs <-
    runResourceT $
    imagePathSource (inputFile params) $$ readImageConduit False =$=
    mergeSource (labelSource (labelFile params)) =$=
    filterConduit' parallelParams fftw filters gFilters False (stride params) =$=
    CL.take (numGMMExample params)
  let (ls, ys) = L.unzip xs
  kmeansModel <-
    kmeans
      parallelParams
      (numGaussian params)
      (kmeansFile params)
      0.005
      (L.concat ys)
  encodeFile (kmeansFile params) kmeansModel
