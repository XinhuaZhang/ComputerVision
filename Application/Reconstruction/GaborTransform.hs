import           Application.Reconstruction.Recon
import           Control.Monad                    as M
import           Control.Monad.Parallel           as MP
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
  (imagePath:imageSizeStr:_) <- getArgs
  let imageSize = read imageSizeStr :: Int
      m = 30
      filterParams =
        GaborFilterParams
        { gaborFilterRows = imageSize
        , gaborFilterCols = imageSize
        , gaborFilterFreq = [1 / 16, 2 / 16 -- , 3 / 16, 4 / 16, 5 / 16, 6 / 16
                            ]
        , gaborFilterScale = [0.25*pi]
        , gaborFilterOrientation = [0,m..180-m]
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
  let imgVecs =
        [VU.convert . VU.map (:+ 0) . removeMean . toUnboxed . computeUnboxedS $ img1] :: [VS.Vector (Complex Double)]
  fftw <- initializefftw fftwWisdom
  filters <- makeFilterConvolution fftw filterParams Normal :: IO GaborFilterConvolution
  conjugateFilters <- makeFilterConvolution fftw filterParams Conjugate :: IO GaborFilterConvolution
  filteredImages <- L.head <$> applyFilterConvolution fftw filters imgVecs
  let cFilterList = getFilterConvolutionList conjugateFilters
      filteredImageList = L.concatMap L.concat filteredImages
  inversedFilteredImageList <-
    M.zipWithM
      (\x y -> do
         y' <- dft2d fftw imageSize imageSize y
         idft2d fftw imageSize imageSize $ VS.zipWith (*) x y')
      cFilterList
      filteredImageList
  let recon = L.foldl1' (VS.zipWith (+)) inversedFilteredImageList
      arr =
        IM.arrayToImage .
        listArray ((0, 0), (imageSize - 1, imageSize - 1)) . VS.toList . normalizeImage (0,1) $
        recon :: IM.ComplexImage
  IM.writeImage "MagnitudeReconComplex.pgm" arr
  IM.writeImage "MagnitudeRecon.pgm" . IM.magnitude $ arr
  -- MP.sequence $
  --   L.zipWith
  --     (\i img ->
  --         IM.writeImage (show i L.++ ".pgm") . IM.magnitude $
  --         (IM.arrayToImage .
  --          listArray ((0, 0), (imageSize - 1, imageSize - 1)) . VS.toList $
  --          img :: IM.ComplexImage))
  --     [1 ..]
  --     -- cFilterList
  --     filteredImageList
