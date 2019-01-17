import Data.Vector as VU
import Data.List as L

{-# INLINE l2norm #-}

l2norm :: VU.Vector Double -> VU.Vector Double
l2norm vec
  | VU.null vec = error "l2norm: empty vector."
  | VU.all (== 0) vec = VU.replicate (VU.length vec) 0
  | otherwise = VU.map (/ norm) vec
  where
    norm = sqrt . VU.sum . VU.map (^ (2 :: Int)) $ vec
