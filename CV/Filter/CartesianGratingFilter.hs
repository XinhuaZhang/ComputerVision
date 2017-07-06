{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
module CV.Filter.CartesianGratingFilter where

import           Control.DeepSeq
import           CV.Filter.GaussianFilter
import           CV.Filter
import           CV.Utility.Coordinates
import           Data.Complex             as C
import           Data.List                as L
import           Data.Vector.Unboxed      as VU
import           Data.Vector.Storable     as VS

data CartesianGratingFilterParams = CartesianGratingFilterParams
  { getCartesianGratingFilterRows             :: !Int
  , getCartesianGratingFilterCols             :: !Int
  , getCartesianGratingFilterScale            :: ![Double]
  , getCartesianGratingFilterFreq             :: ![Double]
  , getCartesianGratingFilterAngle            :: ![Double]
  } deriving (Show)

instance NFData CartesianGratingFilterParams where
  rnf !_ = ()

type CartesianGratingFilterExpansion = Filter CartesianGratingFilterParams [[[VU.Vector (Complex Double)]]]

instance FilterExpansion CartesianGratingFilterExpansion where
  type FilterExpansionInputType CartesianGratingFilterExpansion = [VU.Vector (Complex Double)]
  type FilterExpansionOutputType CartesianGratingFilterExpansion = [[[[Complex Double]]]]
  type FilterExpansionParameters CartesianGratingFilterExpansion = CartesianGratingFilterParams
  type FilterExpansionFilterType CartesianGratingFilterExpansion = VU.Vector (Complex Double)
  {-# INLINE makeFilterExpansion #-}
  makeFilterExpansion params@(CartesianGratingFilterParams rows cols scales freqs oris) rCenter cCenter =
    Filter params $!
    [ [ [ VU.fromListN (rows * cols) $!
         makeFilterExpansionList
           rows
           cols
           rCenter
           cCenter
           (cartesianGrating scale (deg2Rad ori) freq)
        | freq <- freqs ]
      | ori <- oris ]
    | scale <- scales ]
  {-# INLINE getFilterExpansionNum #-}
  getFilterExpansionNum (Filter (CartesianGratingFilterParams _ _ scales freqs oris) _) =
    L.length scales * L.length freqs * L.length oris
  {-# INLINE applyFilterExpansion #-}
  applyFilterExpansion (Filter _ filters) =
    L.map (\x -> L.map (L.map (L.map (VU.sum . VU.zipWith (*) x))) filters)
  {-# INLINE getFilterExpansionList #-}
  getFilterExpansionList = L.concatMap L.concat . getFilter    

{-# INLINE cartesianGrating #-}

cartesianGrating :: Double -> Double -> Double -> Int -> Int -> Complex Double
cartesianGrating scale theta freq x y =
  -- (gaussian2D scale x y :+ 0) *
  exp (0 :+  freq * u)
  where
    c = cos theta
    s = sin theta
    u = fromIntegral x * c - fromIntegral y * s
