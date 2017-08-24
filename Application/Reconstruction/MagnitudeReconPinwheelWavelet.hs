import           Application.Reconstruction.Recon
import           Control.Monad.Trans.Resource
import           CV.IO.ImageIO
import           CV.Filter.PinwheelWavelet
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
  (imagePath:imageSizeStr:lrStr:writeStepStr:_) <- getArgs
  let imageSize = read imageSizeStr :: Int
      m = 90
      n = 8
      filterParams =
        PinwheelWaveletParams
        { pinwheelWaveletRows = imageSize
        , pinwheelWaveletCols = imageSize
        , pinwheelWaveletGaussianScale = 0.5 * pi
        , pinwheelWaveletScale = L.map (\x -> 2 ** (x / 2)) [0 .. 0]
        , pinwheelWaveletRadialScale = L.map (\x -> (1 / sqrt 2) ** x) [0 .. 0]
        , pinwheelWaveletRadialFreqs = L.map (\x  -> x / 8 * pi) [3,4,5] 
        , pinwheelWaveletAngularFreqs = [-7 .. 7]
        , pinwheelWaveletRadius = [5,8 .. 60]
        }
      filters =
        (makeFilterExpansion filterParams (div imageSize 2) (div imageSize 2) :: PinwheelWaveletExpansion)
      inverseFilters =
        (makeFilterExpansion filterParams (div imageSize 2) (div imageSize 2) :: PinwheelWaveletPIExpansion)
  (img:_) <-
    runResourceT $
    sourceList [imagePath] $$ readImageConduit False =$= CL.take 1
  recon <-
    pinwheelWaveletMagnitudeRecon
      imageSize
      imageSize
      (1 * (0.1 ** (read lrStr :: Double)))
      0.001
      (normalizeImageU (-1, 1) . VU.map (:+ 0) . toUnboxed . imageContent $ img)
      filters
      inverseFilters
      (read writeStepStr :: Int)
  let arr =
        IM.arrayToImage .
        listArray ((0, 0), (imageSize - 1, imageSize - 1)) . VU.toList $
        recon :: IM.ComplexImage
  IM.writeImage "MagnitudeReconComplex.pgm" arr
  IM.writeImage "MagnitudeRecon.pgm" . IM.magnitude $ arr
