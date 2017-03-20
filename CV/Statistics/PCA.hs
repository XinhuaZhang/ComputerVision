{-# LANGUAGE BangPatterns #-}

module CV.Statistics.PCA where

import           CV.Utility.Parallel
import           Data.Array.Repa       as R
import           Data.Array.Unboxed    as AU
import           Data.Binary
import           Data.List             as L
import           Data.Vector           as V
import           Data.Vector.Unboxed   as VU
import           Numeric.LinearAlgebra as LA

data PCAMatrix = PCAMatrix
  { pcaMean   :: !(VU.Vector Double)
  , pcaMatrix :: !(V.Vector (VU.Vector Double)) 
  }

instance Binary PCAMatrix where
  put (PCAMatrix m mat) = do
    put . VU.toList $ m
    put . V.toList . V.map VU.toList $ mat
  get = do
    m <- get
    mat <- get
    return $! PCAMatrix (VU.fromList m) (V.fromList . L.map VU.fromList $ mat)

{-# INLINE computeRemoveMean #-}

computeRemoveMean :: ParallelParams
                  -> [VU.Vector Double]
                  -> (VU.Vector Double, [VU.Vector Double])
computeRemoveMean parallelParams xs =
  (mean, parMapChunk parallelParams rdeepseq (removeMean mean) xs)
  where
    !s = L.foldl1' (VU.zipWith (+)) xs
    !mean = VU.map (/ fromIntegral (L.length xs)) s
    
  
{-# INLINE removeMean #-}

removeMean :: VU.Vector Double -> VU.Vector Double -> VU.Vector Double
removeMean mean xs = VU.zipWith (-) xs mean

-- xs are mean-removed
{-# INLINE covarianceMatrix #-}

covarianceMatrix :: ParallelParams -> [VU.Vector Double] -> Matrix Double
covarianceMatrix parallelParams xs = matrix len . R.toList $ rArr
  where
    ys =
      parMapChunk
        parallelParams
        rdeepseq
        (\(i, j) ->
            ( (i, j)
            , (L.sum . L.map (\vec -> (vec VU.! i) * (vec VU.! j)) $ xs) /
              fromIntegral (L.length xs))) .
      L.filter (\(i, j) -> j >= i) $
      [ (i, j)
      | i <- [0 .. len - 1]
      , j <- [0 .. len - 1] ]
    len = VU.length . L.head $ xs
    arr = array ((0, 0), (len - 1, len - 1)) ys :: UArray (Int, Int) Double
    rArr =
      fromFunction
        (Z :. len :. len)
        (\(Z :. i :. j) ->
            if i <= j
              then arr AU.! (i, j)
              else arr AU.! (j, i))

-- xs are mean-removed
{-# INLINE covarianceMatrixP #-}

covarianceMatrixP :: ParallelParams -> [VU.Vector Double] -> IO (Matrix Double)
covarianceMatrixP parallelParams xs = do
  zs <- computeUnboxedP rArr
  return $! matrix len . R.toList $ zs
  where
    !ys =
      parMapChunk
        parallelParams
        rdeepseq
        (\(i, j) ->
            ( (i, j)
            , (L.sum . L.map (\vec -> (vec VU.! i) * (vec VU.! j)) $ xs) /
              fromIntegral (L.length xs))) .
      L.filter (\(i, j) -> j >= i) $
      [ (i, j)
      | i <- [0 .. len - 1]
      , j <- [0 .. len - 1] ]
    len = VU.length . L.head $ xs
    !arr = array ((0, 0), (len - 1, len - 1)) ys :: UArray (Int, Int) Double
    rArr =
      fromFunction
        (Z :. len :. len)
        (\(Z :. i :. j) ->
            if i <= j
              then arr AU.! (i, j)
              else arr AU.! (j, i))

-- compute PCAMatrix and using it to reduce the dimensions of input data
pcaCovariance
  :: ParallelParams
  -> Int
  -> [VU.Vector Double]
  -> IO (PCAMatrix, [VU.Vector Double])
pcaCovariance parallelParams n xs = do
  let (mean, meanRemovedVecs) = computeRemoveMean parallelParams xs
  covMat <- covarianceMatrixP parallelParams meanRemovedVecs
  let (val', vec') = eigSH $ trustSym covMat
      val = LA.toList val'
      vec = L.map (VU.fromList . LA.toList) . toColumns $ vec'
      mat =
        V.fromList . L.take n . snd . L.unzip . L.reverse . L.sortOn fst $
        L.zip val vec
      pcaMat = PCAMatrix mean mat
      reducedVecs =
        parMapChunk
          parallelParams
          rdeepseq
          (\x -> V.convert . V.map (VU.sum . VU.zipWith (*) x) $ mat)
          meanRemovedVecs
  return (pcaMat, reducedVecs)
  
pcaSVD
  :: ParallelParams
  -> Int
  -> [VU.Vector Double]
  -> (PCAMatrix, [VU.Vector Double])
pcaSVD parallelParams n xs = (pcaMat, reducedVecs)
  where
    (mean, meanRemovedVecs) = computeRemoveMean parallelParams xs
    d'' = fromRows . L.map (LA.fromList . VU.toList) $ meanRemovedVecs
    (_, vec', uni') = thinSVD d''
    vec = LA.toList vec'
    uni = L.map (VU.fromList . LA.toList) . toColumns $ uni'
    v = L.take n . snd . L.unzip . L.reverse $ sortOn fst $ L.zip vec uni
    mat = V.fromList v
    pcaMat = PCAMatrix mean mat
    reducedVecs =
      parMapChunk
        parallelParams
        rdeepseq
        (\x -> V.convert . V.map (VU.sum . VU.zipWith (*) x) $ mat)
        meanRemovedVecs


-- perform pcaReduction on uncentered data
pcaReduction :: ParallelParams
             -> PCAMatrix
             -> [VU.Vector Double]
             -> [VU.Vector Double]
pcaReduction parallelParams (PCAMatrix mean mat) =
  parMapChunk
    parallelParams
    rdeepseq
    (\x ->
        let y = removeMean mean x
            z = V.map (VU.sum . VU.zipWith (*) y) mat
        in V.convert z)
