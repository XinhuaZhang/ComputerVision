{-# LANGUAGE TypeFamilies #-}

module CV.FilterExpansion where

import           Control.DeepSeq
import           Data.Complex
import           Data.List           as L
import           Data.Vector.Unboxed

class FilterExpansion a  where
  type FilterParameter a :: *
  makeFilter :: a -> (Int, Int) -> a
  getFilterSize :: a -> Int
  getFilterParameter :: a -> FilterParameter a
  getFilterVectors :: a -> V4SeparableFilter
  changeSizeParameter :: Int -> Int -> a -> a



data V4SeparableFilter
  = V4PolarSeparableFilterAxis [Double]
                               [[Vector (Complex Double)]]
  | V4PolarSeparableFilterGrid ([Double], [Double])
                               [[[Vector (Complex Double)]]]
  | V4CartesianSeparableFilter [Double]
                               [[Vector (Complex Double)]]
  | V4HyperbolicSeparableFilter [Vector (Complex Double)]
  | FourierMellinTransform ([Double], [Double])
                           [[[Vector (Complex Double)]]]
  | Null


data V4SeparableFilterConvolution =
  V4PolarSeparableFilterConvolutionAxis (Int, Int)
                                        [Double]
                                        [[Vector (Complex Double)]]
                                        
instance NFData V4SeparableFilterConvolution where
  rnf (V4PolarSeparableFilterConvolutionAxis a b c) = a `seq` b `seq` c `seq` ()

{-# INLINE grid1D #-}

grid1D :: Int -> Int -> [Int]
grid1D totalLen n = L.take n [len,2 * len .. n * len]
  where len = div totalLen (n + 1)

{-# INLINE grid2D #-}

grid2D :: (Int,Int) -> (Int,Int) -> [(Int,Int)]
grid2D (rows,cols) (nr,nc) = [(j,i)|j <- grid1D rows nr,i <- grid1D cols nc]
