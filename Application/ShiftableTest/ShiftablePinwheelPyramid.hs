{-# LANGUAGE BangPatterns #-}
module Application.ShiftableTest.ShiftablePinwheelPyramid where

import           Control.Monad               as M
import           CV.Array.Image
import           CV.Image
import           CV.Utility.Coordinates
import           CV.Utility.FFT
import           CV.Utility.Parallel
import           CV.Utility.RepaArrayUtility
import           Data.Array.MArray
import           Data.Array.Repa             as R
import           Data.Array.ST
import           Data.Array.Unboxed
import           Data.Complex
import           Data.List                   as L
import           Data.Vector.Storable        as VS
import           Data.Vector.Unboxed         as VU

data ShiftablePinwheelPyramidParams = ShiftablePinwheelPyramidParams
  { shiftablePinwheelPyramidNumLayers   :: Int
  , shiftablePinwheelPyramidNumChannels :: Int
  , shiftablePinwheelPyramidNumTheta    :: Int
  , shiftablePinwheelPyramidNumLogR     :: Int
  } deriving (Show,Read)

data ShiftablePinwheelBlobPyramidParams = ShiftablePinwheelBlobPyramidParams
  { shiftablePinwheelBlobPyramidNumLayers   :: Int
  , shiftablePinwheelBlobPyramidNumChannels :: Int
  , shiftablePinwheelBlobPyramidNumTheta    :: Int
  , shiftablePinwheelBlobPyramidNumLogR     :: Int
  , shiftablePinwheelBlobPyramidK           :: Int
  } deriving (Show,Read)

-- NumChannels X NumTheta X NumLogR
type ShiftablePinwheelPyramidInputArray  = R.Array U DIM3 Double
type ShiftablePinwheelPyramidInternalArray  = R.Array U DIM3 (Complex Double)

data ShiftablePinwheelRingPyramidFilters = ShiftablePinwheelRingPyramidFilters
  { shiftablePinwheelRingPyramidParams :: ShiftablePinwheelPyramidParams
  , shiftablePinwheelRingPyramidH0     :: VS.Vector (Complex Double)
  , shiftablePinwheelRingPyramidL0     :: VS.Vector (Complex Double)
  , shiftablePinwheelRingPyramidH1     :: [VS.Vector (Complex Double)]
  , shiftablePinwheelRingPyramidL1     :: [VS.Vector (Complex Double)]
  }

data ShiftablePinwheelBlobPyramidFilters = ShiftablePinwheelBlobPyramidFilters
  { shiftablePinwheelBlobPyramidParams :: ShiftablePinwheelBlobPyramidParams
  , shiftablePinwheelBlobPyramidH0     :: VS.Vector (Complex Double)
  , shiftablePinwheelBlobPyramidL0     :: VS.Vector (Complex Double)
  , shiftablePinwheelBlobPyramidB      :: [[VS.Vector (Complex Double)]]
  , shiftablePinwheelBlobPyramidL1     :: [VS.Vector (Complex Double)]
  }

generateShiftablePinwheelRingPyramidFilters :: ShiftablePinwheelPyramidParams
                                            -> ShiftablePinwheelRingPyramidFilters
generateShiftablePinwheelRingPyramidFilters params@(ShiftablePinwheelPyramidParams nLayer nChannel nT nR)
  | logBase (2 :: Double) (fromIntegral nR) < fromIntegral nLayer =
    error
      "generateShiftablePinwheelRingPyramidFilters: There are too many layers."
  | otherwise =
    ShiftablePinwheelRingPyramidFilters
      params
      (makeFilter nR h0)
      (makeFilter nR l0)
      (L.map (\i -> makeFilter (div nR (2 ^ i)) h1) [0 .. nLayer - 1])
      (L.map (\i -> makeFilter (div nR (2 ^ i)) l1) [0 .. nLayer - 1])
  where
    makeFilter n op =
      VS.convert .
      toUnboxed . computeS . fromFunction (Z :. nChannel :. nT :. n) $
      (\(Z :. _ :. _ :. i) -> op . fftwFreqOp n $ i)

shiftablePinwheelRingPyramid
  :: FFTW
  -> ShiftablePinwheelRingPyramidFilters
  -> ShiftablePinwheelPyramidInputArray
  -> IO [[R.Array U DIM3 Double]]
shiftablePinwheelRingPyramid fftw (ShiftablePinwheelRingPyramidFilters params@(ShiftablePinwheelPyramidParams _ nChannel nT nR) h0' l0' h1' l1') arr = do
  vec <-
    dft1dG fftw [nChannel, nT, nR] [2] .
    VS.convert . toUnboxed . computeS . R.map (:+ 0) $
    arr
  let h = VS.zipWith (*) h0' vec
      l = VS.zipWith (*) l0' vec
  hVec <- idft1dG fftw [nChannel, nT, nR] [2] h
  let !hFeatures =
        fromUnboxed (Z :. nChannel :. nT :. nR) . VU.map magnitude . VS.convert $
        hVec
  lFeatures <- shiftablePinwheelRingPyramidLoop fftw params 0 h1' l1' l
  return $! [hFeatures] : lFeatures

{-# INLINE shiftablePinwheelRingPyramidLoop #-}

shiftablePinwheelRingPyramidLoop
  :: FFTW
  -> ShiftablePinwheelPyramidParams
  -> Int
  -> [VS.Vector (Complex Double)]
  -> [VS.Vector (Complex Double)]
  -> VS.Vector (Complex Double)
  -> IO [[R.Array U DIM3 Double]]
shiftablePinwheelRingPyramidLoop fftw (ShiftablePinwheelPyramidParams _ nChannel nT nR) n [] [] imgVec = do
  vec <- idft1dG fftw [nChannel, nT, div nR (2 ^ n)] [2] imgVec
  return
    [ [ fromUnboxed (Z :. nChannel :. nT :. div nR (2 ^ n)) .
        VU.map magnitude . VS.convert $
        vec
      ]
    ]
shiftablePinwheelRingPyramidLoop fftw params@(ShiftablePinwheelPyramidParams _ nChannel nT nR) n (h1':h1s) (l1':l1s) imgVec = do
  let h = VS.zipWith (*) h1' imgVec
      l = VS.zipWith (*) l1' imgVec
  hVec <- idft1dG fftw [nChannel, nT, div nR (2 ^ n)] [2] h
  lVec <- idft1dG fftw [nChannel, nT, div nR (2 ^ n)] [2] l
  let !hFeatures =
        fromUnboxed (Z :. nChannel :. nT :. div nR (2 ^ n)) .
        VU.map magnitude . VS.convert $
        hVec
      lArr =
        downsample [2, 1, 1] .
        fromUnboxed (Z :. nChannel :. nT :. div nR (2 ^ n)) . VS.convert $
        lVec
  downsampledFourierLVec <-
    dft1dG fftw [nChannel, nT, div nR (2 ^ (n + 1))] [2] .
    VS.convert . toUnboxed . computeS $
    lArr
  features <- shiftablePinwheelRingPyramidLoop fftw params (n + 1) h1s l1s downsampledFourierLVec
  return $! [hFeatures] : features
shiftablePinwheelRingPyramidLoop _ _ _ _ _ _ =
  error
    "shiftablePinwheelRingPyramidLoop: The lengths of l1 and h1 lists are not the same."

{-# INLINE fftwFreqOp #-}

fftwFreqOp :: Int -> Int -> Double
fftwFreqOp n i
  | i <= div n 2 = 2 * pi * fromIntegral i / fromIntegral n
  | otherwise = 2 * pi * fromIntegral (n - i) / fromIntegral n

{-# INLINE fftwFreqOpBlob #-}

fftwFreqOpBlob :: Int -> Int -> Double
fftwFreqOpBlob n i
  | i <= div n 2 = 2 * pi * fromIntegral i / fromIntegral n
  | otherwise = 2 * pi * fromIntegral (i - n) / fromIntegral n

{-# INLINE h1 #-}

h1 :: Double -> Complex Double
h1 x
  | x >= 0.5 * pi = 1
  | x <= 0.25 * pi = 0
  | otherwise = cos (0.5 * pi * logBase 2 (2 * x / pi)) :+ 0

{-# INLINE l1 #-}

l1 :: Double -> Complex Double
l1 x
  | x >= 0.5 * pi = 0
  | x <= 0.25 * pi = 2
  | otherwise = 2 * cos (0.5 * pi * logBase 2 (4 * x / pi)) :+ 0

{-# INLINE h0 #-}

h0 :: Double -> Complex Double
h0 x = h1 (x / 2)

{-# INLINE l0  #-}

l0 :: Double -> Complex Double
l0 x = l1 (x / 2) / 2

{-# INLINE g1 #-}

g1 :: Int -> Int -> Double -> Complex Double
g1 num k theta
  | abs (theta - pi * fromIntegral k / fromIntegral num) < 0.5 * pi =
    alpha * cos (theta - pi * fromIntegral k / fromIntegral num) ^ (num - 1) :+
    0
  | otherwise = 0
  where
    alpha =
      ((2 :: Double) ^ (num - 1)) * (fromIntegral . L.product $ [2 .. num - 1]) /
      sqrt (fromIntegral $ num * L.product [2 .. 2 * (num - 1)])


cartesian2polarPyramid :: Int
                       -> Int
                       -> Double
                       -> [[R.Array U DIM3 Double]]
                       -> [[R.Array U DIM3 Double]]
cartesian2polarPyramid rows cols polarR =
  L.zipWith (\i x -> L.map (convert2polar i) x) (0 : [0 ..])
  where
    convert2polar n =
      getCartesianImage .
      polar2cartesianImage
        (round (fromIntegral rows / (2 ** n)))
        (round (fromIntegral cols / (2 ** n)))
        (fromIntegral rows / (2 ** (n + 1)), fromIntegral cols / (2 ** (n + 1))) .
      PolarImage ((1 / (2 ** n)) * polarR) (0, 255) .
      computeS . normalizeImage 255
      

cartesian2logpolarPyramid :: Int
                          -> Int
                          -> Double
                          -> [[R.Array U DIM3 Double]]
                          -> [[R.Array U DIM3 Double]]
cartesian2logpolarPyramid rows cols logpolarR =
  L.zipWith (\i x -> L.map (convert2logpolar i) x) (0 : [0 ..])
  where
    convert2logpolar n =
      getCartesianImage .
      logpolar2cartesianImage
        (round (fromIntegral rows / (2 ** n)))
        (round (fromIntegral cols / (2 ** n)))
        (fromIntegral rows / (2 ** (n + 1)), fromIntegral cols / (2 ** (n + 1))) .
      LogpolarImage (log $ (1 / (2 ** n)) * exp logpolarR) (0, 255) .
      computeS . normalizeImage 255


plotShiftablePyramid :: FilePath -> [R.Array U DIM3 Double] -> IO ()
plotShiftablePyramid filePath xs = do
  let (Z :. nf :. r :. _) = extent . L.head $ xs
      numColsList =
        L.map
          (\x ->
              let (Z :. _ :. _ :. y) = extent x
              in y)
          xs
      newC = L.sum numColsList
      imgArr =
        runSTUArray $
        do arr <- newArray ((0, 0, 0), (nf - 1, r - 1, newC - 1)) 0
           M.foldM_
             (\n x -> do
                let (Z :. numBands :. numRows :. numCols) = extent x
                M.mapM_
                  (\(k, j, i) ->
                      writeArray arr (k, j, i + n) (x R.! (Z :. k :. j :. i)))
                  [ (k, j, i)
                  | i <- [0 .. numCols - 1]
                  , j <- [0 .. numRows - 1]
                  , k <- [0 .. numBands - 1] ]
                return $ n + numCols)
             (0 :: Int)
             xs
           return arr
      img = fromListUnboxed (Z :. nf :. r :. newC) . elems $ imgArr
  plotImage (filePath L.++ "_pyramid.png") img
