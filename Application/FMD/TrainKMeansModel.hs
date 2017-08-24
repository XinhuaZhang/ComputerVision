import           Application.FMD.ArgsParser    as AP
import           Application.FMD.PathGenerator
import           Application.Leaf.Conduit
import           Classifier.LibLinear
import           Control.Monad                 as M
import           Control.Monad.Trans.Resource
import           CV.Array.LabeledArray
import           CV.Filter.GaussianFilter
import           CV.Filter.PinwheelWavelet
--import           CV.Filter.MorletWavelet
import           CV.IO.ImageIO
import           CV.Statistics.KMeans
import           CV.Utility.FFT
import           CV.Utility.Parallel           as Par
import           CV.Utility.RepaArrayUtility
import           Data.Array.Repa
import           Data.Binary
import           Data.Complex
import           Data.Conduit
import           Data.Conduit.Binary           as CB
import           Data.Conduit.List             as CL
import           Data.List                     as L
import           Data.Vector.Unboxed           as VU
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
      m = 18
      rows = imageSize params
      cols = (div rows 128) * 96
      filterParams =
        PinwheelWaveletParams
        { pinwheelWaveletRows = rows
        , pinwheelWaveletCols = cols
        , pinwheelWaveletGaussianScale = 0.5 * pi
        , pinwheelWaveletScale = L.map (\x -> 2 ** (x / 1)) [0 .. 2]
        , pinwheelWaveletRadialScale = L.map (\x -> (1 / sqrt 2) ** x) [0 .. 0]
        , pinwheelWaveletRadialFreqs = L.map (\x -> x / 8 * pi) [6,8,10]
        , pinwheelWaveletAngularFreqs = [0 .. 15]
        , pinwheelWaveletRadius = [3..7]
        }
      gFilterParams = GaussianFilterParams (gaussianScale params) rows cols
      fftwWisdom = FFTWWisdomPath (fftwWisdomPath params)
  writeFile (paramsFileName params) . show $ filterParams
  fftwInit <- initializefftw FFTWWisdomNull
  imgs <-
    runResourceT $
    CB.sourceFile (inputFile params) $$ readLabeledImagebinaryConduit =$=
    CL.map
      (VU.map (/ 3) .
       L.foldl1' (VU.zipWith (+)) .
       arrayToUnboxed . (\(LabeledArray _ arr) -> arr)) =$=
    CL.take 1
  generateWisdom fftwInit (fftwWisdomPath params) rows cols .
    VU.convert . VU.map (:+ 0) . L.head $
    imgs
  fftw <- initializefftw fftwWisdom
  filters <-
    makeFilterConvolution fftw filterParams Normal :: IO PinwheelWaveletConvolution -- MorletWaveletConvolution -- PinwheelWaveletConvolution
  gFilters <- makeFilterConvolution fftw gFilterParams Normal :: IO GaussianFilterConvolution
  xs <-
    runResourceT $
    CB.sourceFile (inputFile params) $$ readLabeledImagebinaryConduit =$=
    filterConduit parallelParams fftw [filters] gFilters False (stride params) =$=
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
