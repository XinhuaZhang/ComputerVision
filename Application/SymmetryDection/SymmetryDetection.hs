module Application.SymmetryDection.SymmetryDetection where

import           CV.Filter.PinwheelWavelet
import           CV.Utility.Parallel
import           Data.Array.Repa           as R
import           Data.Complex
import           Data.List                 as L hiding (find)
import           Data.Vector.Storable      as VS hiding (find)


data InflectionPointType
  = Maxima
  | Minima
  deriving (Show)

data Keypoint
  = Keypoint { keypointScaleIndex     :: !Int
             , keypointPositionIndex  :: !Int
             , keypointSymmetryDegree :: !Double}
  | NonKeypoint
  deriving (Show)

{-# INLINE rotationSymmetry #-}

rotationSymmetry :: [Int]
                 -> Int
                 -> [[[VS.Vector (Complex Double)]]]
                 -> [[VS.Vector Double]]
rotationSymmetry afs numPoints xs =
  L.map
    (\scale ->
        L.map
          (\n ->
              L.foldl1' (VS.zipWith (+)) .
              L.zipWith
                (\af z ->
                    VS.map
                      (\a ->
                          (1 -
                           cos
                             (fromIntegral af *
                              (2 * pi / fromIntegral numPoints * fromIntegral n -
                               pi))) *
                          a)
                      z)
                afs $
              scale)
          [0 .. numPoints - 1])
    ys
  where
    ys =
      L.map
        (L.map (L.foldl1' (VS.zipWith (+))) .
         L.transpose . L.map (L.map (VS.map magnitude)))
        xs


{-# INLINE reflectionSymmetry #-}

reflectionSymmetry :: [Int]
                   -> Int
                   -> [[[VS.Vector (Complex Double)]]]
                   -> [[VS.Vector Double]]
reflectionSymmetry afs numPoints =
  L.map
    (\scale ->
        L.map
          (\n ->
              L.foldl1' (VS.zipWith (+)) .
              L.map
                (\bs ->
                    L.foldl1' (VS.zipWith (+)) .
                    L.zipWith3
                      (\af z rz ->
                          VS.zipWith (\a b -> magnitude (a - b)) z .
                          VS.map
                            (\a ->
                                exp
                                  (0 :+
                                   (-2 * fromIntegral af *
                                     (pi / fromIntegral numPoints *
                                      fromIntegral n))) *
                                a) $
                          rz)
                      afs
                      bs .
                    L.reverse $
                    bs) $
              scale)
          [0 .. numPoints - 1])


findInflectionPoint
  :: (Ord a)
  => InflectionPointType -> [a] -> [a]
findInflectionPoint Maxima = find maxima
findInflectionPoint Minima = find minima

{-# INLINE find #-}

find
  :: (Ord a)
  => (a -> a -> a -> Bool) -> [a] -> [a]
find _ [] = []
find _ [_] = []
find _ [_, _] = []
find f (x:y:z:xs)
  | f x y z = y : find f (z : xs)
  | otherwise = find f (y : (z : xs))

{-# INLINE maxima #-}

maxima
  :: (Ord a)
  => a -> a -> a -> Bool
maxima x y z
  | y > x && y > z = True
  | otherwise = False

{-# INLINE minima #-}

minima
  :: (Ord a)
  => a -> a -> a -> Bool
minima x y z
  | y < x && y < z = True
  | otherwise = False


{-# INLINE rotationSymmetryDegree #-}

rotationSymmetryDegree :: [Double] -> Double
rotationSymmetryDegree xs
  | maxVal == 0 = 0
  | otherwise = L.sum (L.map (\y -> maxVal - y) ys) / maxVal
  where
    ys = findInflectionPoint Minima xs
    maxVal = L.maximum xs


{-# INLINE reflectionSymmetryDegree #-}

reflectionSymmetryDegree :: [Double] -> Double
reflectionSymmetryDegree xs
  | maxVal == minVal = 0
  | otherwise = L.sum (L.map (\y -> maxVal - y + minVal) ys) / (maxVal - minVal)
  where
    ys = findInflectionPoint Minima xs
    maxVal = L.maximum xs
    minVal = L.minimum xs


{-# INLINE findKeypoint #-}

findKeypoint :: Double -> R.Array U DIM3 Double -> [Keypoint]
findKeypoint threshold arr' =
  L.filter
    (\x ->
        case x of
          Keypoint {} -> True
          NonKeypoint -> False) .
  R.toList . R.traverse arr' id $
  (\f idx@(Z :. k :. i :. j) ->
      if i < size' || j < size' || i >= nRows - size' || j >= nCols - size'
        then NonKeypoint
        else let index' = [0 .. size'] L.++ [(-size') .. (-1)]
                 x =
                   L.and
                     [ f idx > f (Z :. k - 1 :. i + a :. j + b)
                     | a <- index'
                     , b <- index' ]
                 y =
                   L.and
                     [ f idx > f (Z :. k + 1 :. i + a :. j + b)
                     | a <- index'
                     , b <- index' ]
                 z =
                   L.all (\(a, b) -> f idx > f (Z :. k :. i + a :. j + b)) . L.tail $
                   [ (a, b)
                   | a <- index'
                   , b <- index' ]
             in if ((k == 0 && y) ||
                    (k == nScale - 1 && x) ||
                    (k > 0 && k < nScale - 1 && x && y)) &&
                   z && f idx > threshold
                  then Keypoint k (i * nCols + j) (f idx)
                  else NonKeypoint)
  where
    (Z :. nScale :. nRows :. nCols) = R.extent arr'
    size' = 1
