import           Application.ShiftableTest.ShiftablePinwheelPyramid
import           Control.Monad                                      as M
import           CV.Array.Image
import           CV.IO.ImageIO
import           CV.Utility.FFT
import           Data.Array.Repa                                    as R
import           Data.List                                          as L
import           System.Environment
import           System.FilePath

main = do
  (filePath:_) <- getArgs
  repaImg <- readImageRepa filePath False
  let filterParams =
        ShiftablePinwheelPyramidParams
        { shiftablePinwheelPyramidNumLayers = 3
        , shiftablePinwheelPyramidNumChannels = 1
        , shiftablePinwheelPyramidNumTheta = 128
        , shiftablePinwheelPyramidNumLogR = 128
        }
      (Z :. _ :. rows :. cols) = extent . imageContent $ repaImg
      filters = generateShiftablePinwheelRingPyramidFilters filterParams
      polarR = (sqrt . fromIntegral $ (rows * rows + cols * cols)) / 2
      logpolarR = log polarR
  fftw <- initializefftw FFTWWisdomNull
  resultsPolar <-
    shiftablePinwheelRingPyramid fftw filters .
    getPolarImage .
    cartesian2polarImage
      (shiftablePinwheelPyramidNumTheta filterParams)
      (shiftablePinwheelPyramidNumLogR filterParams)
      (fromIntegral rows / 2, fromIntegral cols / 2)
      polarR .
    CartesianImage (0, 255) . imageContent $
    repaImg
  resultsLogpolar <-
    shiftablePinwheelRingPyramid fftw filters .
    getLogpolarImage .
    cartesian2logpolarImage
      (shiftablePinwheelPyramidNumTheta filterParams)
      (shiftablePinwheelPyramidNumLogR filterParams)
      (fromIntegral rows / 2, fromIntegral cols / 2)
      logpolarR .
    CartesianImage (0, 255) . imageContent $
    repaImg
  let cImagesPolar = L.concatMap (L.map (computeS . normalizeImage 255)) resultsPolar
      cImagesLogpolar = L.concatMap  (L.map (computeS . normalizeImage 255))  resultsLogpolar
  plotShiftablePyramid (dropExtension filePath L.++ "_Polar") cImagesPolar
  plotShiftablePyramid (dropExtension filePath L.++ "_Logpolar") cImagesLogpolar
