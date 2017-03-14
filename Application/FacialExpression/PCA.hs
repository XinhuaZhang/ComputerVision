module Application.FacialExpression.PCA where

import           Data.Array
import           Data.List                  as L
import           Data.Vector.Unboxed        as VU
import           Numeric.LinearAlgebra.Data as LA
import           Numeric.Statistics.PCA

{-# INLINE computePCAMatrix #-}

computePCAMatrix :: Int -> [VU.Vector Double] -> Matrix Double
computePCAMatrix numPrincipal xs = pcaN arr numPrincipal
  where
    arr = listArray (1, L.length xs) . L.map (LA.fromList . VU.toList) $ xs
    
{-# INLINE pca #-}

pca :: Matrix Double
    -> [VU.Vector Double]
    -> [VU.Vector Double]
pca pcaMatrix xs =
  L.map (VU.fromList . LA.toList) .
  elems .
  pcaTransform
    (listArray (1, L.length xs) . L.map (LA.fromList . VU.toList) $ xs) $
  pcaMatrix


