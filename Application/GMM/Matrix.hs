module Application.GMM.Matrix where

import           Data.Matrix         as M
import           Data.Vector         as V
import           Data.Vector.Unboxed as VU
import           Prelude             as P

{-# INLINE crossProduct #-}
crossProduct
  :: (Unbox a, Num a)
  => VU.Vector a -> VU.Vector a
crossProduct vec = VU.concat . P.map (\x -> VU.map (* x) vec) . VU.toList $ vec

{-# INLINE vector2Matrix #-}
vector2Matrix
  :: (Unbox a)
  => Int -> Int -> VU.Vector a -> Matrix a
vector2Matrix nRows nCols vec =
  matrix nRows nCols $ \(i, j) -> vec VU.! ((i - 1) * nRows + (j - 1))

{-# INLINE xtwx #-}
xtwx
  :: (Unbox a, Num a)
  => VU.Vector a -> V.Vector (VU.Vector a) -> a
xtwx x =
  VU.sum . VU.zipWith (*) x . VU.convert . V.map (VU.sum . VU.zipWith (*) x) 
  
{-# INLINE matrixVecMult #-}  
matrixVecMult
  :: (Unbox a, Num a)
  => V.Vector (VU.Vector a) -> VU.Vector a -> VU.Vector a
matrixVecMult mat vec = V.convert . V.map (VU.sum . VU.zipWith (*) vec) $ mat
