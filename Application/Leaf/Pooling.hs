{-# LANGUAGE FlexibleContexts #-}

module Application.Leaf.Pooling
  ( vector2Array
  , getDenseFeatures
  ) where

import           CV.Filter.GaussianFilter
import           CV.Utility.RepaArrayUtility
import           Data.Array.Repa              as R
import           Data.Complex
import           Data.List                    as L
import           Data.Vector.Unboxed          as VU

{-# INLINE gaussianWindow #-}

gaussianWindow
  :: (R.Source s (Double,Double))
  => Array s DIM2 (Double,Double) -> Array D DIM2 (Double,Double)
gaussianWindow arr = R.zipWith (\(m, p) w -> (m * w, p)) arr weight
  where
    (Z :. nRows :. nCols) = extent arr
    rowCenter = (fromIntegral nRows - 1) / 2
    colCenter = (fromIntegral nCols - 1) / 2
    s = 0.5 * fromIntegral (max nRows nCols)
    weight =
      fromFunction (extent arr) $
      \(Z :. j :. i) ->
         gaussian2DDouble
           s
           (fromIntegral j - rowCenter)
           (fromIntegral i - colCenter)

{-# INLINE vector2Array #-}

vector2Array :: (Int, Int)
             -> Vector (Complex Double)
             -> Array U DIM2 (Double, Double)
vector2Array (rows, cols) = fromUnboxed (Z :. rows :. cols) . VU.map polar

{-# INLINE normalizeVec #-}

normalizeVec :: Vector Double -> Vector Double
normalizeVec v =
  let norm = sqrt . VU.sum . VU.map (^ (2 :: Int)) $ v
  in if norm == 0
       then v
       else VU.map (/ norm) v

{-# INLINE orientationHist #-}

orientationHist :: Int -> VU.Vector (Double, Double) -> VU.Vector Double
orientationHist n =
  normalizeVec .
  VU.accumulate (+) (VU.replicate n 0) .
  VU.map
    (\(mag, ori) ->
        let normalizedOri =
              if ori < 0 || ori >= (2 * pi)
                then if ori < 0
                       then ori + 2 * pi
                       else ori - 2 * pi
                else ori
            idx = floor (fromIntegral n * normalizedOri / (2 * pi))
            idx1 =
              if idx >= n
                then n - 1
                else idx
        in (idx1, mag))

getDenseFeatures
  :: (R.Source s (Double, Double))
  => Int -> Int -> Int -> Array s DIM2 (Double, Double) -> [Vector Double]
getDenseFeatures patchSize stride n arr =
  L.map
    (\start ->
        let patch = cropUnsafe start [patchSize, patchSize] arr
        in orientationHist n . toUnboxed . computeS . gaussianWindow $ patch)
    startPairList
  where
    (Z :. rows :. cols) = extent arr
    startPairList =
      [ [c, r]
      | c <- computeStartPointList cols
      , r <- computeStartPointList rows ]
    computeStartPointList x =
      let num = div x stride
          margin = div (x - stride * num) 2
      in L.filter (\y -> y + patchSize <= x) $
         L.take num [margin,margin + stride .. x]
