{-# LANGUAGE TypeFamilies #-}
module CV.Filter
  ( module CV.Utility.DFT
  , module CV.Filter
  ) where

import           Control.DeepSeq      (NFData, rnf)
import           CV.Utility.DFT
import           CV.Utility.FFT       (FFTW)
import           Data.Complex
import           Data.List            as L
import           Data.Vector.Storable as VS
import           Data.Vector.Unboxed  as VU

data Filter a b
  = Filter { getFilterParams :: !a
           , getFilter       :: !b}

data ConvolutionalFilterType
  = Normal
  | Conjugate
  deriving (Show, Read)


instance (NFData a, NFData b) =>
         NFData (Filter a b) where
  rnf (Filter x y) = rnf x `seq` rnf y `seq` ()

class FilterExpansion a where
  type FilterExpansionParameters a :: *
  getFilterExpansionNum :: a -> Int
  getFilterExpansionList :: a -> [VU.Vector (Complex Double)]
  makeFilterExpansion :: FilterExpansionParameters a -> Int -> Int -> a
  applyFilterExpansion :: a -> [VU.Vector (Complex Double)] -> [Complex Double]

class FilterConvolution a where
  type FilterConvolutionParameters a
  getFilterConvolutionNum :: a -> Int
  getFilterConvolutionList :: a -> [VS.Vector (Complex Double)]
  makeFilterConvolution :: DFTPlan
                        -> FilterConvolutionParameters a
                        -> ConvolutionalFilterType
                        -> IO (DFTPlan, a)
  applyFilterConvolution
    :: DFTPlan
    -> a
    -> [VS.Vector (Complex Double)]
    -> IO [VS.Vector (Complex Double)]
  applyInvariantFilterConvolution
    :: DFTPlan
    -> a
    -> [VS.Vector (Complex Double)]
    -> IO [[VS.Vector (Complex Double)]]

{-# INLINE makeFilterExpansionList #-}

makeFilterExpansionList :: Int -> Int -> Int -> Int -> (Int -> Int -> a) -> [a]
makeFilterExpansionList rows cols rCenter cCenter f =
  [ f (r - rCenter) (c - cCenter)
  | r <- [0 .. rows - 1]
  , c <- [0 .. cols - 1] ]

{-# INLINE makeFilterExpansionListPeriod #-}

makeFilterExpansionListPeriod :: Int -> Int -> Int -> Int -> (Int -> Int -> a) -> [a]
makeFilterExpansionListPeriod rows cols rCenter cCenter f =
  [ let func x center len =
          L.minimumBy
            (\a b -> compare (abs a) (abs b))
            [x - center, x - center + len, x - center - len]
    in f (func r rCenter rows) (func c cCenter cols)
  | r <- [0 .. rows - 1]
  , c <- [0 .. cols - 1]
  ]

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
    Normal    -> id
    Conjugate -> Prelude.map conjugate
