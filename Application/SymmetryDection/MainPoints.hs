import           Application.SymmetryDection.SymmetryDetection
import           Control.Monad                                 as M
import           Control.Monad.Parallel                        as MP
import           CV.Filter.PinwheelWavelet
import           CV.IO.ImageIO
import           CV.Utility.Draw
import           CV.Utility.FFT
import           CV.Utility.Parallel
import           CV.Utility.RepaArrayUtility
import           Data.Array.Repa                               as R
import           Data.Complex
import           Data.List                                     as L
import           Data.Vector.Storable                          as VS
import           Data.Vector.Unboxed                           as VU
import           Graphics.Rendering.Chart.Backend.Cairo
import           Graphics.Rendering.Chart.Easy
import           System.Environment
import           Text.Printf

main = do
  (imagePath:prefixStr:_) <- getArgs
  let n = 128 -- 32 --128
      filterParams =
        PinwheelWaveletParams
        { pinwheelWaveletRows = n
        , pinwheelWaveletCols = n
        , pinwheelWaveletGaussianScale = 0.5
        , pinwheelWaveletScale = L.map (\x -> sqrt 2 ** x) [0 .. 3]
        , pinwheelWaveletRadialScale = L.map (\x -> ((2 ** (-x / 2)))) [0 .. 3]
        , pinwheelWaveletRadialFreqs = 3 / 4 * pi
        , pinwheelWaveletAngularFreqs = [-7 .. 7]
        , pinwheelWaveletRadius = [1 .. 4]
        }
      path = "fftw.dat"
      fftwWisdom = FFTWWisdomPath path
  fftwInit <- initializefftw FFTWWisdomNull
  img <-
    fmap
      (\x ->
          let xs = arrayToUnboxed . imageContent $ x
              imageVec =
                VU.map (/ (fromIntegral . L.length $ xs)) .
                L.foldl1' (VU.zipWith (+)) . L.map (VU.map (:+ 0)) $
                xs
          in VU.convert imageVec) $
    readImageRepa imagePath True
  generateWisdom fftwInit path n n img
  fftw <- initializefftw fftwWisdom
  image <- readImageRepa imagePath True
  filters <- makeFilterConvolution fftw filterParams Normal :: IO PinwheelWaveletConvolution
  let (Z :. nf :. _ :. _) = extent . imageContent $ image
      imageVecs =
        L.map (VU.convert . VU.map (:+ 0)) . arrayToUnboxed . imageContent $ image
  filteredImages <- applyPinwheelWaveletConvolution fftw filters imageVecs
  let numPoints = 50
      normalizedFilteredImages =
        L.map
          (L.map
             (parMap
                rdeepseq
                (VS.map
                   (\x ->
                       if (magnitude x) < 10 ** (-6)
                         then 0 :+ 0
                         else x))))
          filteredImages
      rotation =
        L.map
          (parMap rdeepseq rotationSymmetryDegree . L.transpose . L.map VS.toList) .
        rotationSymmetry (pinwheelWaveletAngularFreqs filterParams) numPoints $
        -- filteredImages
        normalizedFilteredImages
      reflection =
        L.map
          (parMap rdeepseq reflectionSymmetryDegree . L.transpose . L.map VS.toList) .
        reflectionSymmetry (pinwheelWaveletAngularFreqs filterParams) numPoints $
        -- filteredImages
        normalizedFilteredImages
      threshold = 15
      degree = L.concat $ L.zipWith (L.zipWith (+)) rotation reflection
      points =
        L.groupBy (\a b -> keypointScaleIndex a == keypointScaleIndex b) .
        L.sortOn keypointScaleIndex .
        findKeypoint threshold .
        R.fromListUnboxed
          (Z :. (L.length . pinwheelWaveletRadialScale $ filterParams) :. n :. n) $
        degree
      circles =
        L.map
          (L.map
             (\(Keypoint _ idx _) ->
                 let i = div idx n
                     j = mod idx n
                 in Circle 1 1 (fromIntegral i, fromIntegral j)))
          points
      scales = L.map (keypointScaleIndex . L.head) points
      scaleVec = VU.fromList . pinwheelWaveletRadialScale $ filterParams
  -- position =
  --   L.map (\(Keypoint _ idx' _) -> (div idx' n, mod idx' n)) . L.head $ points
  MP.sequence_ $
    L.zipWith
      (\s c ->
          plotImageRepa
            (prefixStr L.++ "Points_" L.++ (printf "%.4f" (scaleVec VU.! s)) L.++
             ".png")
            (draw image Yellow c))
      scales
      circles
