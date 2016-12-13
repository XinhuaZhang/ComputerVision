{-# LANGUAGE BangPatterns #-}
module Application.FilterStats.FilterStats where

import           Data.List                              as L
import           Data.Maybe
import           Data.Vector.Unboxed                    as VU
import           Graphics.Rendering.Chart.Backend.Cairo
import           Graphics.Rendering.Chart.Easy


{-# INLINE valueRange #-}
valueRange :: VU.Vector Double -> (Double,Double)
valueRange vec = (VU.minimum vec, VU.maximum vec)

{-# INLINE meanVar #-}
meanVar :: VU.Vector Double -> (Double,Double)
meanVar vec = (mu, sigma)
  where
    !len = VU.length vec
    !mu = VU.sum vec / fromIntegral len
    !sigma = 
      (VU.sum . VU.map (\x -> (x - mu) ^ (2 :: Int)) $ vec) / fromIntegral len

plotHist :: VU.Vector Double
         -> (Double, Double)
         -> Int
         -> String
         -> FilePath
         -> IO ()
plotHist vec (a, b) nbins title filePath =
  toFile def filePath $
  do layout_title .= "Histogram"
     plot (line title [VU.toList . VU.zip indexVec $ hist])
  where
    !width = (b - a) / fromIntegral nbins
    !indexVec = VU.generate nbins (\i -> a + fromIntegral i * width)
    !eleIndexVec =
      VU.filter (\x -> x >= 0 || x <= nbins - 1) .
      VU.map (\x -> floor $ (x - a) / width :: Int) $
      vec
    !hist =
      VU.accumulate (+) (VU.replicate nbins 0) . VU.zip eleIndexVec $
      VU.replicate (VU.length indexVec) (1 :: Int)
