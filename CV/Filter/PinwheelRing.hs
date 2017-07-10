{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}

module CV.Filter.PinwheelRing
  ( module F
  , PinwheelRingParams(..)
  , PinwheelRingExpansion
  ) where

import           Control.Monad          as M
import           CV.Filter              as F
import           CV.Utility.Coordinates
import           CV.Utility.FFT
import           Data.Complex           as C
import           Data.List              as L
import           Data.Vector.Storable   as VS
import           Data.Vector.Unboxed    as VU


data PinwheelRingParams = PinwheelRingParams
  { pinwheelRingRows         :: !Int
  , pinwheelRingCols         :: !Int
  , pinwheelGaussianScale    :: !Double
  , pinwheelRingScale        :: ![Double]
  , pinwheelRingRadialFreqs  :: !Double
  , pinwheelRingAngularFreqs :: ![Int]
  , pinwheelRingRadius       :: ![Double]
  } deriving (Show, Read)

type PinwheelRingExpansion = Filter PinwheelRingParams [[[VU.Vector (Complex Double)]]]
type PinwheelRingConvolution = Filter PinwheelRingParams [[[VS.Vector (Complex Double)]]]

instance FilterExpansion PinwheelRingExpansion where
  type FilterExpansionParameters PinwheelRingExpansion = PinwheelRingParams
  {-# INLINE makeFilterExpansion #-}
  makeFilterExpansion params@(PinwheelRingParams rows cols gScale scales rf afs radiuses) rCenter cCenter =
    Filter params $!
    [ [ [ VU.fromListN (rows * cols) $
         makeFilterExpansionList
           rows
           cols
           rCenter
           cCenter
           (pinwheelRing gScale scale rf af radius)
        | radius <- radiuses ]
      | af <- afs ]
    | scale <- scales ]
  {-# INLINE getFilterExpansionNum #-}
  getFilterExpansionNum (Filter (PinwheelRingParams _ _ _ scales _ afs radiuses) _) =
    L.length scales * L.length afs * L.length radiuses
  {-# INLINE applyFilterExpansion #-}
  applyFilterExpansion (Filter _ filters) =
    L.concatMap
      (\x -> L.concatMap (L.concatMap (L.map (VU.sum . VU.zipWith (*) x))) filters)
  {-# INLINE getFilterExpansionList #-}
  getFilterExpansionList = L.concatMap L.concat . getFilter
  
{-# INLINE pinwheelRing #-}

pinwheelRing :: Double -> Double -> Double -> Int -> Double -> Int -> Int -> Complex Double
pinwheelRing gScale scale rf af shift x y
  | x == 0 && y == 0 = 0
  | otherwise =
    (gaussian gScale r' r :+ 0) / (sqrt scale :+ 0) *
    exp
      (0 :+ fromIntegral af * angleFunctionRad (fromIntegral x) (fromIntegral y)) *
    radialFunc rf r' r
  where
    r = (log . sqrt . fromIntegral $ x ^ (2 :: Int) + y ^ (2 :: Int)) / scale
    r' = log shift / scale 

{-# INLINE radialFunc #-}

radialFunc :: Double -> Double -> (Double -> Complex Double)
radialFunc rFreq r' r = exp (0 :+ rFreq * (r - r'))

{-# INLINE gaussian #-}

gaussian
  :: (Floating a, Ord a)
  => a -> a -> a -> a
gaussian sd r' r = exp (-(r - r') ^ (2 :: Int) / (2 * (sd ^ (2 :: Int))))
