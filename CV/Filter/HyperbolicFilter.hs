{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
module CV.Filter.HyperbolicFilter where

import           Control.DeepSeq
import           CV.Filter.GaussianFilter
import           CV.FilterExpansion
import           CV.Utility.Coordinates
import           Data.Complex             as C
import           Data.List                as L
import           Data.Vector.Unboxed      as VU


-- data HyperbolicFilterParams = HyperbolicFilterParams
--   { getHyperbolicFilterGridRows         :: !Int
--   , getHyperbolicFilterGridCols         :: !Int
--   , getHyperbolicFilterRows             :: !Int
--   , getHyperbolicFilterCols             :: !Int
--   , getHyperbolicFilterDownsampleFactor :: !Int
--   , getHyperbolicFilterScale            :: ![Double]
--   , getHyperbolicFilterFreq             :: ![Double]
--   , getHyperbolicFilterAngle            :: ![Double]
--   } deriving (Show)

-- instance NFData HyperbolicFilterParams where
--   rnf !_ = ()

data HyperbolicSeparableFilterParams = HyperbolicSeparableFilterParams
  { getHyperbolicSeparableFilterRows             :: !Int
  , getHyperbolicSeparableFilterCols             :: !Int
  , getHyperbolicSeparableFilterScale            :: ![Double]
  , getHyperbolicSeparableFilterUFreq            :: ![Int]
  , getHyperbolicSeparableFilterVFreq            :: ![Int]
  , getHyperbolicSeparableFilterAngle            :: ![Double]
  } deriving (Show)

instance NFData HyperbolicSeparableFilterParams where
  rnf !_ = ()

-- data HyperbolicFilter = HyperbolicFilter
--   { getHyperbolicFilterParams :: HyperbolicFilterParams
--   , getHyperbolicFilter       :: [[VU.Vector (Complex Double)]]
--   }

-- instance NFData HyperbolicFilter where
--   rnf !_ = ()

data HyperbolicSeparableFilter = HyperbolicSeparableFilter
  { getHyperbolicSeparableFilterParams :: HyperbolicSeparableFilterParams
  , getHyperbolicSeparableFilter       :: V4SeparableFilter
  }

instance NFData HyperbolicSeparableFilter where
  rnf !_ = ()

-- instance FilterExpansion HyperbolicFilter where
--   type FilterParameter HyperbolicFilter = HyperbolicFilterParams
--   type FilterVectors HyperbolicFilter = [[VU.Vector (Complex Double)]]
--   {-# INLINE makeFilter #-}
--   makeFilter (HyperbolicFilter params@(HyperbolicFilterParams gRows gCols rows cols downsampleFactor scales freqs angles) _) =
--     HyperbolicFilter params .
--     L.map
--       (\(centerR, centerC) ->
--           [ VU.fromListN
--              (newCols * newRows)
--              [ hyperbolic scale angle freq (x - centerC) (y - centerR)
--              | y <- [0 .. newRows - 1]
--              , x <- [0 .. newCols - 1] ]
--           | angle <- radAngles
--           , scale <- scales
--           , freq <- freqs ]) $
--     grid2D (newRows, newCols) (gRows, gCols)
--     where
--       newCols = div cols downsampleFactor
--       newRows = div rows downsampleFactor
--       radAngles = L.map deg2Rad angles
--   getFilterSize (HyperbolicFilter (HyperbolicFilterParams gRows gCols _ _ _ scales fs as) _) =
--     gRows * gCols * (L.product . L.map L.length $ [scales, fs, as])
--   getFilterParameter = getHyperbolicFilterParams
--   {-# INLINE getFilterVectors #-}
--   getFilterVectors (HyperbolicFilter _ vecs) = vecs
--   {-# INLINE changeSizeParameter #-}
--   changeSizeParameter rows cols (HyperbolicFilter (HyperbolicFilterParams gRows gCols _ _ df scale freq angle) vecs) =
--     HyperbolicFilter
--       (HyperbolicFilterParams gRows gCols rows cols df scale freq angle)
--       vecs

-- {-# INLINE hyperbolic #-}

-- hyperbolic :: Double -> Double -> Double -> Int -> Int -> Complex Double
-- hyperbolic scale theta freq x y =
--   (gaussian2D scale x y :+ 0) *
--   exp (0 :+ pi * freq * (sqrt . abs $! (x' * y')) / scale) -- /
--   -- (scale :+ 0)
--   where
--     c = cos theta
--     s = sin theta
--     x' = fromIntegral x * c - fromIntegral y * s
--     y' = fromIntegral x * s + fromIntegral y * c



instance FilterExpansion HyperbolicSeparableFilter where
  type FilterParameter HyperbolicSeparableFilter = HyperbolicSeparableFilterParams
  {-# INLINE makeFilter #-}
  makeFilter (HyperbolicSeparableFilter params@(HyperbolicSeparableFilterParams rows cols scales uFreqs vFreqs angles) _) =
    HyperbolicSeparableFilter params . V4HyperbolicSeparableFilter . L.concat $
    [ L.concat
      [ [ VU.fromListN
        (cols * rows)
        [ hyperbolicSeparable
          scale
          angle
          uFreq
          vFreq
          (x - centerC)
          (y - centerR)
        | y <- [0 .. rows - 1]
        , x <- [0 .. cols - 1]
        ]
      | uFreq <- uFreqs
      ]
      | vFreq <- vFreqs
      ] L.++
    L.concat
      [ [ VU.fromListN
        (cols * rows)
        [ hyperbolicSeparableC
          scale
          angle
          uFreq
          vFreq
          (x - centerC)
          (y - centerR)
        | y <- [0 .. rows - 1]
        , x <- [0 .. cols - 1]
        ]
      | uFreq <- uFreqs
      ]
      | vFreq <- vFreqs
      ]
    | scale <- scales
    , angle <- radAngles
    ]
    where
      centerC = div cols 2
      centerR = div rows 2
      radAngles = L.map deg2Rad angles
  getFilterSize (HyperbolicSeparableFilter (HyperbolicSeparableFilterParams _ _ scales ufs vfs as) _) =
    L.product [L.length scales, L.length ufs, L.length vfs, L.length as]
  getFilterParameter = getHyperbolicSeparableFilterParams
  {-# INLINE getFilterVectors #-}
  getFilterVectors (HyperbolicSeparableFilter _ vecs) = vecs
  {-# INLINE changeSizeParameter #-}
  changeSizeParameter rows cols (HyperbolicSeparableFilter (HyperbolicSeparableFilterParams _ _ scale uFreq vFreq angle) vecs) =
    HyperbolicSeparableFilter
      (HyperbolicSeparableFilterParams rows cols scale uFreq vFreq angle)
      vecs

{-# INLINE hyperbolicSeparable #-}

hyperbolicSeparable :: Double
                    -> Double
                    -> Int
                    -> Int
                    -> Int
                    -> Int
                    -> Complex Double
hyperbolicSeparable scale theta uFreq vFreq x y
  | x' == 0 || y' == 0 =
    (gaussian2DDouble scale x' y' :+ 0) 
  | otherwise =
    (gaussian2DDouble scale x' y' :+ 0) *
    exp (0 :+ pi * fromIntegral vFreq * v / scale) *
    exp (0 :+ 2 *  pi * fromIntegral uFreq * u / (abs . log . sqrt $ 2 * scale) )
  where
    c = cos theta
    s = sin theta
    x' = fromIntegral x * c - fromIntegral y * s
    y' = fromIntegral x * s + fromIntegral y * c
    u = log . sqrt . abs $! x' / y'
    v = sqrt . abs $! x' * y'



{-# INLINE hyperbolicSeparableC #-}

hyperbolicSeparableC :: Double
                     -> Double
                     -> Int
                     -> Int
                     -> Int
                     -> Int
                     -> Complex Double
hyperbolicSeparableC scale theta uFreq vFreq x y
  | x' == 0 || y' == 0 =
    (gaussian2DDouble scale x' y' :+ 0) 
  | otherwise =
    (gaussian2DDouble scale x' y' :+ 0) *
    (conjugate $ exp (0 :+ pi * fromIntegral vFreq * v / scale) )*
    exp (0 :+ 2 *  pi * fromIntegral uFreq * u / (abs . log . sqrt $ 2 * scale) )
  where
    c = cos theta
    s = sin theta
    x' = fromIntegral x * c - fromIntegral y * s
    y' = fromIntegral x * s + fromIntegral y * c
    u = log . sqrt . abs $! x' / y'
    v = sqrt . abs $! x' * y'
