module Application.ShiftableTest.LaplacianPyramid where

import           Control.Monad      as M
import           Data.Array.MArray
import           Data.Array.ST
import           Data.Array.Unboxed
import           Data.Image
import           Data.List          as L

{-# INLINE reduceKernel #-}

reduceKernel :: [Double]
reduceKernel = [0.05, 0.25, 0.4, 0.25, 0.05]

{-# INLINE projectKernel #-}

projectKernel :: [Double]
projectKernel = [0.1, 0.5, 0.8, 0.5, 0.1]

{-# INLINE project #-}

project :: GrayImage -> GrayImage
project =
  convolveRows projectKernel . upsampleRows . convolveCols projectKernel . upsampleCols

{-# INLINE reduce #-}

reduce :: GrayImage -> GrayImage
reduce =
  downsampleRows . convolveRows reduceKernel . downsampleCols . convolveCols reduceKernel

laplacianPyramid :: Int -> GrayImage -> [GrayImage]
laplacianPyramid maxIteration = loop 1
  where
    loop n x =
      if n == maxIteration || (odd . rows $ x) || (odd . cols $ x)
        then [x]
        else let y = reduce x
             in (x - project y) : loop (n + 1) y


plotLaplacianPyramid :: FilePath -> [GrayImage] -> IO ()
plotLaplacianPyramid filePath xs = do
  let r = rows . L.head $ xs
      c = cols . L.head $ xs
      newC = L.foldl' (\b a -> b + div c (2 ^ a)) c [1 .. L.length xs - 1]
      imgArr =
        runSTUArray $
        do arr <- newArray ((0, 0), (r - 1, newC - 1)) 0
           M.foldM_
             (\(k, n) z -> do
                M.mapM_
                  (\(a, b) -> writeArray arr (a, k + b) (ref z a b))
                  [ (i, j)
                  | i <- [0 .. div r (2 ^ n) - 1]
                  , j <- [0 .. div c (2 ^ n) - 1] ]
                return (k + div c (2 ^ n), n + 1))
             (0, 0)
             xs
           return arr
      img = makeImage r newC (\i j -> imgArr ! (i, j)) :: GrayImage
  writeImage (filePath L.++ "_LaplacianPyramid.pgm") img
