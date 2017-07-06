{-# LANGUAGE TypeFamilies #-}
module CV.Filter where

import           Control.DeepSeq      (NFData, rnf)
import           CV.Utility.FFT       (FFTW)
import           Data.Complex
import           Data.Vector.Storable as VS
import           Data.Vector.Unboxed  as VU

data Filter a b
  = Filter { getFilterParams :: !a
           , getFilter       :: !b}

data ConvolutionalFilterType
  = Normal
  | Conjugate
  deriving (Show, Read)

instance Functor (Filter a) where
  fmap f (Filter x y) = Filter x $! f y

instance (NFData a, NFData b) =>
         NFData (Filter a b) where
  rnf (Filter x y) = rnf x `seq` rnf y `seq` ()

class FilterExpansion a  where
  type FilterExpansionParameters a :: *
  getFilterExpansionNum :: a -> Int
  getFilterExpansionList :: a -> [VU.Vector (Complex Double)]
  makeFilterExpansion :: FilterExpansionParameters a -> Int -> Int -> a
  applyFilterExpansion :: a -> [VU.Vector (Complex Double)] -> [Complex Double]

class FilterConvolution a  where
  type FilterConvolutionParameters a :: *
  getFilterConvolutionNum :: a -> Int
  getFilterConvolutionList :: a -> [VS.Vector (Complex Double)]
  makeFilterConvolution :: FFTW
                        -> FilterConvolutionParameters a
                        -> ConvolutionalFilterType
                        -> IO a
  applyFilterConvolution :: FFTW
                         -> a
                         -> [VS.Vector (Complex Double)]
                         -> IO [VS.Vector (Complex Double)]

{-# INLINE makeFilterExpansionList #-}

makeFilterExpansionList :: Int -> Int -> Int -> Int -> (Int -> Int -> a) -> [a]
makeFilterExpansionList rows cols rCenter cCenter f =
  [ f (r - rCenter) (c - cCenter)
  | r <- [0 .. rows - 1]
  , c <- [0 .. cols - 1] ]

{-# INLINE makeFilterConvolutionList #-}

makeFilterConvolutionList :: Int -> Int -> (Int -> Int -> a) -> [a]
makeFilterConvolutionList rows cols f =
  [ let x =
          if r < (rows `div` 2)
            then r
            else r - rows
        y =
          if c < (cols `div` 2)
            then c
            else c - cols
    in f x y
  | r <- [0 .. rows - 1]
  , c <- [0 .. cols - 1] ]


{-# INLINE conjugateFunc #-}

conjugateFunc :: ConvolutionalFilterType
              -> ([Complex Double] -> [Complex Double])
conjugateFunc x =
  case x of
    Normal -> id
    Conjugate -> Prelude.map conjugate
