{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
module CV.FilterConvolution where

import           Control.DeepSeq
import           Data.Complex
import           Data.List            as L
import           Data.Vector.Storable as VS
import           Data.Vector.Unboxed  as VU


class FilterConvolution a  where
  type FilterConvolutionParamsType a :: *
  makeConvolutionFilter :: FilterConvolutionParamsType a -> a
  getConvolutionFilterSize :: a -> Int
  changeSizeParameterConvolution :: Int -> Int -> a -> a


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
  rnf (V4PolarSeparableFilteredImageConvolutionAxis a b c) =
    a `seq` b `seq` c `seq` ()
  rnf (FourierMellinTransformFilteredImageConvolution a b c) =
    a `seq` b `seq` c `seq` ()


instance Show V4SeparableFilteredImageConvolution where
  show (V4PolarSeparableFilteredImageConvolutionAxis a b c) =
    (show a) L.++ "\n" L.++ (show b) L.++ "\n" L.++
    (show . VU.length . L.head . L.head $ c)
  show (FourierMellinTransformFilteredImageConvolution a b c) =
    (show a) L.++ "\n" L.++ (show b) L.++ "\n" L.++
    (show . VU.length . L.head . L.head . L.head $ c)

{-# INLINE makeFilterList #-}

makeFilterList :: Int -> Int -> (Int -> Int -> a) -> [a]
makeFilterList rows cols f =
  [ let !x =
          if r < (rows `div` 2)
            then r
            else r - rows
        !y =
          if c < (cols `div` 2)
            then c
            else c - cols
    in f x y
  | r <- [0 .. rows - 1]
  , c <- [0 .. cols - 1]
  ]
