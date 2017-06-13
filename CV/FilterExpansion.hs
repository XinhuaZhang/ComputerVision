{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}

module CV.FilterExpansion where

import           Control.DeepSeq
import           Data.Complex
import           Data.List            as L
import           Data.Vector.Storable as VS
import           Data.Vector.Unboxed  as VU

class FilterExpansion a  where
  type FilterParameter a :: *
  makeFilter :: a -> (Int, Int) -> a
  getFilterSize :: a -> Int
  getFilterParameter :: a -> FilterParameter a
  getFilterVectors :: a -> V4SeparableFilter
  changeSizeParameter :: Int -> Int -> a -> a



data V4SeparableFilter
  = V4PolarSeparableFilterAxis [Double]
                               [[VU.Vector (Complex Double)]]
  | V4PolarSeparableFilterGrid ([Double], [Double])
                               [[[VU.Vector (Complex Double)]]]
  | V4CartesianSeparableFilter [Double]
                               [[VU.Vector (Complex Double)]]
  | V4HyperbolicSeparableFilter [VU.Vector (Complex Double)]
  | FourierMellinTransform ([Double], [Double])
                           [[[VU.Vector (Complex Double)]]]
  | Null

instance NFData V4SeparableFilter where
  rnf !_ = ()


data V4SeparableFilterConvolution
  = V4PolarSeparableFilterConvolutionAxis !(Int, Int)
                                          ![Double]
                                          ![[VS.Vector (Complex Double)]]
  | FourierMellinTransformConvolution (Int, Int)
                                      ([Double], [Double])
                                      [[[VS.Vector (Complex Double)]]]

instance NFData V4SeparableFilterConvolution where
  rnf (V4PolarSeparableFilterConvolutionAxis a b c) = a `seq` b `seq` c `seq` ()
  rnf (FourierMellinTransformConvolution a b c) = a `seq` b `seq` c `seq` ()

data V4SeparableFilteredImageConvolution
  = V4PolarSeparableFilteredImageConvolutionAxis !(Int, Int)
                                                 ![Double]
                                                 ![[VU.Vector (Complex Double)]]
  | FourierMellinTransformFilteredImageConvolution (Int, Int)
                                                   ([Double], [Double])
                                                   [[[VU.Vector (Complex Double)]]]

instance NFData V4SeparableFilteredImageConvolution where
  rnf !_ = ()
  -- rnf (V4PolarSeparableFilteredImageConvolutionAxis a b c) = ()
  --   a `seq` b `seq` c `seq` ()
  -- rnf (FourierMellinTransformFilteredImageConvolution a b c) =
  --   a `seq` b `seq` c `seq` ()


instance Show V4SeparableFilteredImageConvolution where
  show (V4PolarSeparableFilteredImageConvolutionAxis a b c) =
    (show a) L.++ "\n" L.++ (show b) L.++ "\n" L.++
    (show . VU.length . L.head . L.head $ c)
  show (FourierMellinTransformFilteredImageConvolution a b c) =
    (show a) L.++ "\n" L.++ (show b) L.++ "\n" L.++
    (show . VU.length . L.head . L.head . L.head $ c)



{-# INLINE grid1D #-}

grid1D :: Int -> Int -> [Int]
grid1D totalLen n = L.take n [len,2 * len .. n * len]
  where len = div totalLen (n + 1)

{-# INLINE grid2D #-}

grid2D :: (Int,Int) -> (Int,Int) -> [(Int,Int)]
grid2D (rows,cols) (nr,nc) = [(j,i)|j <- grid1D rows nr,i <- grid1D cols nc]
