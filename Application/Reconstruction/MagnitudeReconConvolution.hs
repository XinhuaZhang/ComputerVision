import           Application.Reconstruction.Recon
import           Control.Monad                    as M
import           Control.Monad.Trans.Resource
import           CV.IO.ImageIO
import           CV.Utility.FFT
import           CV.V4Filter
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
  (imagePath:imageSizeStr:thresholdStr:_) <- getArgs
  let imageSize = read imageSizeStr :: Int
      m = 90
      n = 4
      filterParams =
        FourierMellinTransformParamsGrid
        { getFourierMellinTransformGridRows = imageSize
        , getFourierMellinTransformGridCols = imageSize
        , getFourierMellinTransformGridScale = [0]
        , getFourierMellinTransformGridRadialFreq = [0 .. fromIntegral n]
        , getFourierMellinTransformGridAngularFreq = [0 .. n]
        }
      nullFilter = PolarSeparableFilter filterParams Null
      filters =
        (\(FourierMellinTransform _ vecs) -> L.concatMap L.concat vecs) . getFilterVectors $
        makeFilter nullFilter (div imageSize 2, div imageSize 2)
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
  let imgVec1 = VU.convert . VU.map (:+ 0) . toUnboxed . computeUnboxedS $ img1
  fftw <- initializefftw fftwWisdom
  filtersF <- M.mapM (dft2d fftw imageSize imageSize . VU.convert) filters
  recon <-
    magnitudeReconConvolution
      fftw
      imageSize
      imageSize
      (1 * (0.1 ** 20))
      (read thresholdStr :: Double)
      imgVec1
      filtersF
  let arr =
        IM.arrayToImage .
        listArray ((0, 0), (imageSize - 1, imageSize - 1)) . VU.toList $
        recon :: IM.ComplexImage
  IM.writeImage "MagnitudeReconComplex.pgm" arr
  IM.writeImage "MagnitudeRecon.pgm" . IM.magnitude $ arr
