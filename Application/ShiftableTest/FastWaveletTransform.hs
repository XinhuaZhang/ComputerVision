{-# LANGUAGE FlexibleContexts #-}
module Application.ShiftableTest.FastWaveletTransform where

import           Control.Monad      as M
import           Data.Array.MArray
import           Data.Array.ST
import           Data.Array.Unboxed
import           Data.Complex
import           Data.Image         hiding (Complex)
import           Data.List          as L

cvdw10H0 :: [Complex Double]
cvdw10H0 =
  [ 0
  , 0.01049245951230 :+ (-0.02059043708702)
  , (-0.01712890812780) :+ (-0.00872852869034)
  , (-0.08063970414533) :+ 0.11794747353812
  , 0.15137970843150 :+ 0.09422365674476
  , 0.64300323451588 :+ (-0.18285216450551)
  , 0.64300323451588 :+ (-0.18285216450551)
  , 0.15137970843150 :+ 0.09422365674476
  , (-0.08063970414533) :+ 0.11794747353812
  , (-0.01712890812780) :+ (-0.00872852869034)
  , 0.01049245951230 :+ (-0.02059043708702)
  ]

cvdw10H1 :: [Complex Double]
cvdw10H1 =
  [ 0
  , 0.01049245951230 :+ 0.02059043708702
  , 0.01712890812780 :+ (-0.00872852869034)
  , (-0.08063970414533) :+ (-0.11794747353812)
  , (-0.15137970843150) :+ 0.09422365674476
  , 0.64300323451588 :+ 0.18285216450551
  , (-0.64300323451588) :+ (-0.18285216450551)
  , 0.15137970843150 :+ (-0.09422365674476)
  , 0.08063970414533 :+ 0.11794747353812
  , (-0.01712890812780) :+ 0.00872852869034
  , (-0.01049245951230) :+ (-0.02059043708702)
  ]

daubechies4H0 :: [Double]
daubechies4H0 =
  [ 0
  , (1 + root3) / root2
  , (3 + root3) / root2
  , (3 - root3) / root2
  , (1 - root3) / root2
  ]
  where
    root2 = 4 * sqrt 2
    root3 = sqrt 3

daubechies4H1 :: [Double]
daubechies4H1 =
  [ 0
  , (1 - root3) / root2
  , (root3 - 3) / root2
  , (3 + root3) / root2
  , -(1 + root3) / root2
  ]
  where
    root2 = 4 * sqrt 2
    root3 = sqrt 3
    
rowsDown
  :: (Image img, Num (Pixel img))
  => [Pixel img] -> img -> img
rowsDown filter' = downsampleRows . convolveRows filter'

colsDown
  :: (Image img, Num (Pixel img))
  => [Pixel img] -> img -> img
colsDown filter' = downsampleCols . convolveCols filter'

rowsUp
  :: (Image img, Num (Pixel img), Monoid (Pixel img))
  => [Pixel img] -> img -> img
rowsUp filter' = convolveRows filter' . upsampleRows


fastWaveletTransform
  :: (Image img, Num (Pixel img), Monoid (Pixel img))
  => Int -> [Pixel img] -> [Pixel img] -> img -> [[img]]
fastWaveletTransform maxIteration h0 h1 img = loop 1 img
  where
    len = min (L.length h0) (L.length h1)
    loop n x =
      let r = rows x
          c = cols x
      in if n == maxIteration || odd r || odd c || r < len || c < len
           then [[x]]
           else let l = rowsDown h0 x
                    h = rowsDown h1 x
                in [colsDown h1 l, colsDown h0 h, colsDown h1 h] :
                   loop (n + 1) (colsDown h0 l)
                   

plotFastWaveletTransform :: FilePath -> [[GrayImage]] -> IO ()
plotFastWaveletTransform filePath xs = do
  let r = rows . L.head . L.head $ xs
      c = cols . L.head . L.head $ xs
      imgArr =
        runSTUArray $
        do arr <- newArray ((0, 0), (2 * r - 1, 2 * c - 1)) 0
           M.foldM_
             (\((x, y), n) zs -> do
                let origins =
                      [(x, y), (x + div r (2 ^ n), y), (x, y + div c (2 ^ n))]
                if L.length zs == 1
                  then M.mapM_
                         (\(a, b) ->
                             writeArray arr (x + a, x + b) (ref (L.head zs) a b))
                         [ (i, j)
                         | i <- [0 .. div r (2 ^ (n - 1)) - 1]
                         , j <- [0 .. div c (2 ^ (n - 1)) - 1] ]
                  else M.zipWithM_
                         (\(oa, ob) z ->
                             M.mapM_
                               (\(a, b) ->
                                   writeArray arr (oa + a, ob + b) (ref z a b))
                               [ (i, j)
                               | i <- [0 .. div r (2 ^ n) - 1]
                               , j <- [0 .. div c (2 ^ n) - 1] ])
                         origins
                         zs
                return ((x + div r (2 ^ n), y + div c (2 ^ n)), n + 1))
             ((0, 0), 0 :: Int)
             xs
           return arr
      img = makeImage (2 * r) (2 * c) (\i j -> imgArr ! (i, j)) :: GrayImage
  writeImage (filePath L.++ "_fwtpyramid.pgm") img
