import           Control.Monad             as M
import           CV.Filter.PinwheelWavelet
import           CV.Utility.Coordinates
import           Data.Array                as Arr
import qualified Data.Image                as IM
import           Data.List                 as L
import           Data.Vector.Unboxed       as VU
import           System.Directory
import           System.Environment
import           System.FilePath
import           Text.Printf

main = do
  (nameStr:_) <- getArgs -- name: Fan, Ring and Blob
  let rows = 64
      cols = 64
      filterParams =
        case nameStr of
          "Ring" ->
            PinwheelWaveletRingParams
              PinwheelRingParams
              { pinwheelRingRows = rows
              , pinwheelRingCols = cols
              , pinwheelRingGaussianScale = 0.1 * pi
              , pinwheelRingScale = L.map (\x -> 2 ** (x / 4)) [0 .. 2]
              , pinwheelRingRadialFreqs = [7] -- L.map (\x -> x / 8 * pi) []
              , pinwheelRingAngularFreqs = [7]
              , pinwheelRingRadius = [4]
              }
          "Fan" ->
            PinwheelWaveletFanParams
              PinwheelFanParams
              { pinwheelFanRows = rows
              , pinwheelFanCols = cols
              , pinwheelFanGaussianScale = 0.05 * pi
              , pinwheelFanScale = L.map (\x -> 2 ** (x / 2)) [0 .. 3]
              , pinwheelFanRadialFreqs = L.map (\x -> x / 8 * pi) [6, 8, 10, 12]
              , pinwheelFanAngularFreqs = [3]
              , pinwheelFanTheta = L.map (* (2 * pi)) [0.5, 1]
              }
          "Blob" ->
            PinwheelWaveletBlobParams
              PinwheelBlobParams
              { pinwheelBlobRows = rows
              , pinwheelBlobCols = cols
              , pinwheelBlobGaussianScale = 0.5 * pi
              , pinwheelBlobScale = [1]
              , pinwheelBlobFreqs = 0.5 * pi
              , pinwheelBlobOrientation = [16]
              , pinwheelBlobThetaShift = [16] -- [0,127]
              , pinwheelBlobRadiusShift = [22,24,26,28] -- [0,63]
              }
          _ -> error "The name is not in {Fan, Ring, Blob}."
      getFilterFunc (PinwheelWaveletFilterExpansionFan filters) =
        getFilterExpansionList filters
      getFilterFunc (PinwheelWaveletFilterExpansionRing filters) =
        getFilterExpansionList filters
      getFilterFunc (PinwheelWaveletFilterExpansionBlob filters) =
        getFilterExpansionList filters
      filters = getFilterFunc $ makePinwheelWaveletFilterExpansion filterParams
      imgList =
        L.map
          (IM.arrayToImage .
           listArray ((0, 0), (rows - 1, cols - 1)) . VU.toList)
          filters :: [IM.ComplexImage]
  createDirectoryIfMissing True "PinwheelWavelet"
  removePathForcibly ("PinwheelWavelet" </> nameStr)
  createDirectoryIfMissing True ("PinwheelWavelet" </> nameStr)
  M.zipWithM_
    (\i img ->
       IM.writeImage
         ("PinwheelWavelet" </> nameStr </> (printf "%s_%03d.pgm" nameStr i))
         img)
    [(1 :: Int) ..]
    imgList
