import           Application.Reconstruction.Recon
import           Control.Monad                    as M
import           Control.Monad.Trans.Resource
import           CV.Filter.PolarSeparableFilter
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
import System.Random

generateImage :: Int -> Int -> Double -> Double -> IO (VU.Vector Double)
generateImage rows cols minVal maxVal =
  VU.fromListN (rows * cols) <$>
  M.replicateM (rows * cols) (randomRIO (minVal, maxVal))


main = do
  (imageSizeStr:thresholdStr:lrStr:writeStepStr:_) <- getArgs
  let imageSize = read imageSizeStr :: Int
      n = 8
      filterParams =
        PolarSeparableFilterParamsGrid
        { getPolarSeparableFilterGridRows = imageSize
        , getPolarSeparableFilterGridCols = imageSize
        , getPolarSeparableFilterGridScale = [1]
        , getPolarSeparableFilterGridRadialFreq = [0 .. 15]
        , getPolarSeparableFilterGridAngularFreq = [0 .. 15]
        }
      fftwWisdomPath = "fftw.dat"
      fftwWisdom = FFTWWisdomPath fftwWisdomPath
  img <- generateImage imageSize imageSize 0 255
  let imgVec = VU.convert . VU.map (:+ 0) $ img
  fftwInit <- initializefftw FFTWWisdomNull
  generateWisdom fftwInit fftwWisdomPath imageSize imageSize imgVec -- This resets imgVec
  img1 <- generateImage imageSize imageSize 0 255
  let imgVec1 = normalizeImage (-1, 1) . VU.convert . VU.map (:+ 0) $ img1
      imgArr =
        IM.arrayToImage .
        listArray ((0, 0), (imageSize - 1, imageSize - 1)) . VU.toList $
        img1 :: IM.GrayImage
  fftw <- initializefftw fftwWisdom
  filters <- makeFilterConvolution fftw filterParams Normal :: IO PolarSeparableFilterGridConvolution
  IM.writeImage "RandomImage.pgm" imgArr
  recon <-
    magnitudeReconConvolution
      fftw
      imageSize
      imageSize
      (1 * (0.1 ** (read lrStr :: Double)))
      (read thresholdStr :: Double)
      imgVec1
      (getFilterConvolutionList filters)
      NULL
      (read writeStepStr :: Int)
      "Random_"
  let arr =
        IM.arrayToImage .
        listArray ((0, 0), (imageSize - 1, imageSize - 1)) . VU.toList $
        recon :: IM.ComplexImage
      
  IM.writeImage "MagnitudeReconRandom.pgm" . IM.magnitude $ arr
  
