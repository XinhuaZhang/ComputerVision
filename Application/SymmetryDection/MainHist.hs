import           Application.SymmetryDection.SymmetryDetection
import           Control.Monad                                 as M
import           CV.Filter.PinwheelWavelet
import           CV.IO.ImageIO
import           CV.Statistics.Histogram
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
  (imagePath:_) <- getArgs
  let point = (64, 64) -- (16,16) -- (100, 54)
      idx = (round $ fst point - 1) * n + (round $ snd point)
      alpha = L.head . pinwheelWaveletRadialScale $ filterParams
      r =
        round $
        (L.last . pinwheelWaveletRadius $ filterParams) / alpha +
        3 * (pinwheelWaveletGaussianScale filterParams) *
        (L.last . pinwheelWaveletScale $ filterParams) /
        alpha
      n = 128 -- 32 --128
      filterParams =
        PinwheelWaveletParams
        { pinwheelWaveletRows = n
        , pinwheelWaveletCols = n
        , pinwheelWaveletGaussianScale = 0.5
        , pinwheelWaveletScale = L.map (\x -> sqrt 2 ** x) [0 .. 3]
        , pinwheelWaveletRadialScale =
          L.map (\x -> (2 ** (-1 / 3)) ** x) [0 .. 3]
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
          let (Z :. nf' :. _ :. _) = extent . imageContent $ x
              imageVec =
                VU.map (/ (fromIntegral nf')) .
                L.foldl1' (VU.zipWith (+)) .
                L.map
                  (\i ->
                      toUnboxed .
                      computeS . R.map (:+ 0) . R.slice (imageContent x) $
                      (Z :. i :. All :. All)) $
                [0 .. nf' - 1]
          in VU.convert imageVec) $
    readImageRepa imagePath True
  generateWisdom fftwInit path n n img
  fftw <- initializefftw fftwWisdom
  image <- readImageRepa imagePath True
  filters <- makeFilterConvolution fftw filterParams Normal :: IO PinwheelWaveletConvolution
  let (Z :. nf :. _ :. _) = extent . imageContent $ image
      imageVecs =
        L.map
          (\i ->
              VU.convert .
              toUnboxed . computeS . R.map (:+ 0) . R.slice (imageContent image) $
              (Z :. i :. All :. All))
          [0 .. nf - 1]
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
        normalizedFilteredImages
      reflection =
        L.map
          (parMap rdeepseq reflectionSymmetryDegree . L.transpose . L.map VS.toList) .
        reflectionSymmetry (pinwheelWaveletAngularFreqs filterParams) numPoints $
        normalizedFilteredImages
      histParams = KdHistParams 500 (1 / 10) False 1
      degree =
        L.zipWith
          (\a b ->
              L.map (\(Bin _ x) -> x) .
              histogram .
              build histParams . L.map VU.singleton . L.zipWith (+) a $
              b)
          rotation
          reflection
  print $ L.zipWith (\a b -> L.maximum . L.zipWith (+) a $ b) rotation reflection
  M.zipWithM
    (\xs s ->
        toFile def ("histogram_" L.++ (printf "%.4f" s) L.++ ".png") $
        plot
          (line
             ""
             [ L.zip
                 (L.map (\i -> i * (binWidth histParams)) [(0 :: Double) ..])
                 xs
             ]))
    degree
    (pinwheelWaveletRadialScale filterParams)
