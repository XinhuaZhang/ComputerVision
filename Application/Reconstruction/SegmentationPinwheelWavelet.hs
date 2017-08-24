import           Application.Reconstruction.Recon
import           Control.Monad                    as M
import           Control.Monad.Trans.Resource
import           CV.Filter.GaussianFilter
import           CV.Filter.PinwheelWavelet
import           CV.IO.ImageIO
import           CV.Utility.FFT
import           Data.Array                       as Arr
import           Data.Array.Repa                  as R
import           Data.Complex
import           Data.Conduit
import           Data.Conduit.List                as CL
import qualified Data.Image                       as IM
import           Data.List                        as L
import           Data.Vector.Unboxed              as VU
import           System.Environment

main = do
  (imagePath:imageSizeStr:thresholdStr:gaussianScaleStr:lrStr:lambdaStr:writeStepStr:_) <-
    getArgs
  let imageSize = read imageSizeStr :: Int
      gaussianScale = read gaussianScaleStr :: Double
      lambda = read lambdaStr :: Double
      n = 8
      filterParams =
        PinwheelWaveletParams
        { pinwheelWaveletRows = imageSize
        , pinwheelWaveletCols = imageSize
        , pinwheelWaveletGaussianScale = 0.5 * pi
        , pinwheelWaveletScale = L.map (\x -> 2 ** (x / 1)) [0 .. 0]
        , pinwheelWaveletRadialScale = L.map (\x -> (1 / sqrt 2) ** x) [0 .. 0]
        , pinwheelWaveletRadialFreqs = L.map (\x -> x / 8 * pi) [3, 4, 5]
        , pinwheelWaveletAngularFreqs = [0 .. 7]
        , pinwheelWaveletRadius = [5,10..30]
        }
      gFilterParams = GaussianFilterParams [gaussianScale] imageSize imageSize
      fftwWisdomPath = "fftw.dat"
      fftwWisdom = FFTWWisdomPath fftwWisdomPath
  (img:_) <-
    runResourceT $
    sourceList [imagePath] $$ readImageConduit False =$= CL.take 1
  let imgVec = VU.convert . VU.map (:+ 0) . toUnboxed . imageContent $ img
  fftwInit <- initializefftw FFTWWisdomNull
  generateWisdom fftwInit fftwWisdomPath imageSize imageSize imgVec -- This resets imgVec
  (img1:_) <-
    runResourceT $
    sourceList [imagePath] $$ readImageConduit False =$= CL.take 1
  let imgVec1 =
        normalizeImage (-1, 1) . VU.convert . VU.map (:+ 0) . toUnboxed . imageContent $
        img1
  fftw <- initializefftw fftwWisdom
  filters <- makeFilterConvolution fftw filterParams Normal :: IO PinwheelWaveletConvolution
  filtersPI <- makeFilterConvolution fftw filterParams Normal :: IO PinwheelWaveletPIConvolution
  gFilters <- makeFilterConvolution fftw gFilterParams Normal :: IO GaussianFilterConvolution
  recon <-
    magnitudeSegmentation
      fftw
      imageSize
      imageSize
      lambda
      (1 * (0.1 ** (read lrStr :: Double)))
      (read thresholdStr :: Double)
      imgVec1
      (getFilterConvolutionList filters)
      (getFilterConvolutionList filtersPI)
      gFilters
      NULL
      (read writeStepStr :: Int)
      ""
  let arr =
        IM.arrayToImage .
        listArray ((0, 0), (imageSize - 1, imageSize - 1)) . VU.toList $
        recon :: IM.ComplexImage
  IM.writeImage "MagnitudeReconComplex.pgm" arr
  IM.writeImage "MagnitudeRecon.pgm" . IM.magnitude $ arr
