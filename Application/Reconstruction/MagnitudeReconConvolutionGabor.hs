import           Application.Reconstruction.Recon
import           Control.Monad                    as M
import           Control.Monad.Trans.Resource
import           CV.Filter.Gabor
import           CV.IO.ImageIO
import           CV.Utility.FFT
import           Data.Array                       as Arr
import           Data.Array.Repa                  as R
import           Data.Complex
import           Data.Conduit
import           Data.Conduit.List                as CL
import qualified Data.Image                       as IM
import           Data.List                        as L
import           Data.Vector.Storable             as VS
import           Data.Vector.Unboxed              as VU
import           System.Environment


main = do
  (imagePath:initReconPath:imageSizeStr:thresholdStr:lrStr:_) <- getArgs
  let imageSize = read imageSizeStr :: Int
      m = 15
      n = (3 * pi / 4) / 7
      filterParams =
        GaborFilterParams
        { gaborFilterRows = imageSize
        , gaborFilterCols = imageSize
        , gaborFilterFreq = [1 / 16, 2 / 16 , 3 / 16, 4 / 16, 5 / 16, 6 / 16
                            ]
        , gaborFilterScale = [0.25*pi]
        , gaborFilterOrientation = [0,m .. 180 - m]
        }
      fftwWisdomPath = "fftw.dat"
      fftwWisdom = FFTWWisdomPath fftwWisdomPath
  (img:_) <-
    runResourceT $
    sourceList [imagePath] $$ readImageConduit False =$= CL.take 1
  let imgVec = VU.convert . VU.map (:+ 0) . toUnboxed . computeUnboxedS $ img
  fftwInit <- initializefftw FFTWWisdomNull
  generateWisdom fftwInit fftwWisdomPath imageSize imageSize imgVec -- This resets imgVec
  (img1:_) <-
    runResourceT $
    sourceList [imagePath] $$ readImageConduit False =$= CL.take 1
  let imgVec1 = VU.convert . VU.map (:+ 0) . removeMean . toUnboxed . computeUnboxedS $ img1
  fftw <- initializefftw fftwWisdom
  filters <- makeFilterConvolution fftw filterParams Normal :: IO GaborFilterConvolution
  initRecon <-
    if L.null initReconPath
      then return NULL
      else do
        (img2:_) <-
          runResourceT $
          sourceList [initReconPath] $$ readImageConduit False =$= CL.take 1
        return $!
          InitRecon . VU.convert . VU.map (:+ 0) . removeMean . toUnboxed . computeUnboxedS $
          img2
  recon <-
    magnitudeReconConvolution
      fftw
      imageSize
      imageSize
      (1 * (0.1 ** (read lrStr :: Double)))
      (read thresholdStr :: Double)
      imgVec1
      (getFilterConvolutionList filters)
      initRecon
  let m = imageMean . toUnboxed . computeUnboxedS $ img1
      arr =
        IM.arrayToImage .
        listArray ((0, 0), (imageSize - 1, imageSize - 1)) . VS.toList . normalizeImage (0,1) . VU.convert . VU.map (+(m :+ 0)) $
        recon :: IM.ComplexImage
      arr1 =
        IM.arrayToImage .
        listArray ((0, 0), (imageSize - 1, imageSize - 1)) . VS.toList .  VU.convert $
        imgVec1 :: IM.ComplexImage
  -- IM.writeImage "MagnitudeReconComplex.pgm" arr
  IM.writeImage "MagnitudeRecon.pgm" . IM.realPart $ arr
  IM.writeImage "Image.pgm" . IM.realPart $ arr1
