module Application.GMM.Matrix where

import           Data.Matrix         as M
import           Data.Vector.Unboxed as VU
import           Prelude             as P


crossProduct :: (Unbox a,Num a)
             => VU.Vector a -> VU.Vector a
crossProduct vec = VU.concat . P.map (\x -> VU.map (* x) vec) . VU.toList $ vec

vector2Matrix
  :: (Unbox a)
  => Int -> Int -> VU.Vector a -> Matrix a
vector2Matrix nRows nCols vec =
  matrix nRows nCols $ \(i,j) -> vec VU.! ((i - 1) * nRows + (j - 1))
