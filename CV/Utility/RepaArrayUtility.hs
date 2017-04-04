{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
module CV.Utility.RepaArrayUtility where

import           Data.Array.Repa              as R
import           Data.Array.Repa.Stencil      as R
import           Data.Array.Repa.Stencil.Dim2 as R
import           Data.List                    as L
import           Data.Vector                  as V
import           Data.Vector.Unboxed          as VU
import           Prelude                      as P
import           Data.Complex

-- factor = 2^n, n = 0,1,..
-- the first factor in the list corresponds to the inner-most (right-most) dimension.
downsample
  :: (Source s e
     ,Shape sh)
  => [Int] -> Array s sh e -> Array D sh e
downsample factorList arr
  | L.all (== 1) factorList = delay arr
  | L.any (< 1) newDList = error "Downsample factors are too large."
  | otherwise =
    R.backpermute (shapeOfList newDList)
                  (shapeOfList . L.zipWith (*) factorList . listOfShape)
                  arr
  where dList = listOfShape . extent $ arr
        newDList = L.zipWith div dList factorList

downsampleUnsafe
  :: (Source s e
     ,Shape sh)
  => [Int] -> Array s sh e -> Array D sh e
downsampleUnsafe factorList arr =
  R.backpermute newSh
              (shapeOfList . L.zipWith (*) factorList . listOfShape)
              arr
  where dList = listOfShape $ extent arr
        newSh = shapeOfList $ L.zipWith div dList factorList

crop
  :: (Source s e
     ,Shape sh)
  => [Int] -> [Int] -> Array s sh e -> Array D sh e
crop start len arr
  | L.any (< 0) start ||
      L.or (L.zipWith3 (\x y z -> x > (z - y))
                       start
                       len
                       dList) =
    error $
    "Crop out of boundary!\n" L.++ show start L.++ "\n" L.++ show len L.++ "\n" L.++
    show dList
  | otherwise =
    R.backpermute (shapeOfList len)
                (shapeOfList . L.zipWith (+) start . listOfShape)
                arr
  where dList = listOfShape $ extent arr

cropUnsafe
  :: (Source s e
     ,Shape sh)
  => [Int] -> [Int] -> Array s sh e -> Array D sh e
cropUnsafe start len =
  R.backpermute (shapeOfList len)
              (shapeOfList . L.zipWith (+) start . listOfShape)

pad :: (Real e
       ,Source s e
       ,Shape sh)
    => [Int] -> Array s sh e -> Array D sh e
pad newDims arr =
  backpermuteDft
    (fromFunction (shapeOfList dimList) (\_ -> 0))
    (\sh' ->
       let idx = L.zipWith (-) (listOfShape sh') diff
       in if L.or (L.zipWith (\i j -> i < 0 || (i >= j)) idx oldDimList)
            then Nothing
            else Just $ shapeOfList idx)
    arr
  -- fromFunction
  --   (shapeOfList dimList)
  --   (\sh' ->
  --      let idx = L.zipWith (-) (listOfShape sh') diff
  --      in if L.or (L.zipWith (\i j -> i < 0 || (i >= j)) idx oldDimList)
  --            then 0
  --            else arr R.! shapeOfList idx)
  where
    oldDimList = listOfShape . extent $ arr
    dimList = L.zipWith max newDims oldDimList
    diff =
      L.zipWith
        (\a b ->
           if a - b <= 0
             then 0
             else div (a - b) 2)
        newDims
        oldDimList


computeDerivativeP
  :: Array U DIM2 Double -> IO [Array U DIM2 Double]
computeDerivativeP arr =
  do let xStencil =
           [stencil2| 0 0 0
                      -1 0 1
                      0 0 0 |]
         yStencil =
           [stencil2| 0 -1 0
                      0 0 0
                      0 1 0 |]
         xyStencil =
           [stencil2| 1 0 -1
                      0 0 0
                      -1 0 1 |]
         ds =
           L.map (\s -> R.map (/ 2) $ mapStencil2 BoundClamp s arr)
                 [xStencil,yStencil,xyStencil]
     ds' <- P.mapM computeP ds
     return $! (arr : ds')

computeDerivativeS
  :: Array U DIM2 Double -> [Array U DIM2 Double]
computeDerivativeS arr = arr : ds'
  where xStencil =
          [stencil2| 0 0 0
                     -1 0 1
                     0 0 0 |]
        yStencil =
          [stencil2| 0 -1 0
                     0 0 0
                     0 1 0 |]
        xyStencil =
          [stencil2| 1 0 -1
                     0 0 0
                     -1 0 1 |]
        ds =
          L.map (\s -> R.map (/ 2) $ mapStencil2 BoundClamp s arr)
                [xStencil,yStencil,xyStencil]
        ds' = L.map computeS ds

{-# INLINE bicubicInterpolation #-}
bicubicInterpolation
  :: [Array U DIM2 Double] -> (Double,Double) ->  (Double,Double) -> Double
bicubicInterpolation ds (minVal, maxVal) (y, x)
  | (x < 0) ||
      (x > (fromIntegral nx - 1)) || (y < 0) || (y > (fromIntegral ny - 1)) = 0
  | result < minVal = minVal
  | result > maxVal = maxVal
  | otherwise = result
  where
    (Z :. ny :. nx) = extent . P.head $ ds
    x' = x - fromIntegral (floor x :: Int)
    y' = y - fromIntegral (floor y :: Int)
    idx =
      VU.fromListN
        4
        [ (floor y, floor x)
        , (floor y, ceiling x)
        , (ceiling y, floor x)
        , (ceiling y, ceiling x)
        ] :: VU.Vector (Int, Int)
    xs =
      VU.concat .
      P.map (\arr' -> VU.map (\(i, j) -> arr' R.! (Z :. i :. j)) idx) $
      ds
    alpha = V.map (VU.sum . VU.zipWith (*) xs) matrixA
    arr = fromListUnboxed (Z :. 4 :. 4) . V.toList $ alpha :: R.Array U DIM2 Double
    arr1 =
      R.traverse arr id (\f idx'@(Z :. j :. i) -> f idx' * (x' ^ i) * (y' ^ j))
    result = sumAllS arr1
    matrixA =
      V.fromListN 16 . P.map (VU.fromListN 16) $
      [ [1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
      , [0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
      , [-3, 3, 0, 0, -2, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
      , [2, -2, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
      , [0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0]
      , [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0]
      , [0, 0, 0, 0, 0, 0, 0, 0, -3, 3, 0, 0, -2, -1, 0, 0]
      , [0, 0, 0, 0, 0, 0, 0, 0, 2, -2, 0, 0, 1, 1, 0, 0]
      , [-3, 0, 3, 0, 0, 0, 0, 0, -2, 0, -1, 0, 0, 0, 0, 0]
      , [0, 0, 0, 0, -3, 0, 3, 0, 0, 0, 0, 0, -2, 0, -1, 0]
      , [9, -9, -9, 9, 6, 3, -6, -3, 6, -6, 3, -3, 4, 2, 2, 1]
      , [-6, 6, 6, -6, -3, -3, 3, 3, -4, 4, -2, 2, -2, -2, -1, -1]
      , [2, 0, -2, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0]
      , [0, 0, 0, 0, 2, 0, -2, 0, 0, 0, 0, 0, 1, 0, 1, 0]
      , [-6, 6, 6, -6, -4, -2, 4, 2, -3, 3, -3, 3, -2, -1, -2, -1]
      , [4, -4, -4, 4, 2, 2, -2, -2, 2, -2, 2, -2, 1, 1, 1, 1]
      ]

{-# INLINE rescale2D #-}

rescale2D
  :: (Source s Double)
  => (Int, Int) -> (Double, Double) -> Array s DIM2 Double -> Array D DIM2 Double
rescale2D (newNy, newNx) bound arr =
  fromFunction
    (Z :. newNy :. newNx)
    (\(Z :. j :. i) ->
        bicubicInterpolation
          ds
          bound
          (fromIntegral j * ratioY, fromIntegral i * ratioX))
  where
    ds = computeDerivativeS . computeUnboxedS . delay $ arr
    (Z :. ny' :. nx') = extent arr
    ratioX = fromIntegral (nx' - 1) / fromIntegral (newNx - 1) 
    ratioY = fromIntegral (ny' - 1) / fromIntegral (newNy - 1) 

{-# INLINE rescale25D #-}

rescale25D
  :: (Source s Double)
  => (Int, Int) -> (Double, Double) -> Array s DIM3 Double -> Array U DIM3 Double
rescale25D newSize@(nx',ny') bound arr =
  fromUnboxed (Z :. nf' :. ny' :. nx') .
  VU.concat .
  L.map (\i ->
           toUnboxed . computeUnboxedS . rescale2D newSize bound . R.slice arr $
           (Z :. i :. All :. All)) $
  [0 .. nf' - 1]
  where (Z :. nf' :. _ :. _) = extent arr

{-# INLINE extractPointwiseFeature #-}

extractPointwiseFeature
  :: (R.Source s e, Unbox e)
  => R.Array s DIM3 e -> [VU.Vector e]
extractPointwiseFeature arr' =
  [ toUnboxed . computeUnboxedS . R.slice arr' $ (Z :. All :. j :. i)
  | j <- [0 .. ny' - 1]
  , i <- [0 .. nx' - 1] ]
  where
    !(Z :. _ :. (ny'::Int) :. (nx'::Int)) = extent arr'

{-# INLINE extractPointwiseComplexFeature #-}

extractPointwiseComplexFeature
  :: (R.Source s (Complex Double))
  => R.Array s DIM3 (Complex Double) -> [VU.Vector Double]
extractPointwiseComplexFeature arr' =
  [ VU.fromListN (2 * nf') .
   L.concatMap (\(a :+ b) -> [a, b]) .
   VU.toList . toUnboxed . computeUnboxedS . R.slice arr' $
   (Z :. All :. j :. i)
  | j <- [0 .. ny' - 1]
  , i <- [0 .. nx' - 1] ]
  where
    !(Z :. nf' :. (ny' :: Int) :. (nx' :: Int)) = extent arr'

{-# INLINE extractFeatureMap #-}

extractFeatureMap
  :: (R.Source s e)
  => R.Array s DIM3 e -> [[e]]
extractFeatureMap arr' =
  L.map (\k -> R.toList . R.slice arr' $ (Z :. k :. All :. All)) [0 .. nf' - 1]
  where
    !(Z :. (nf' :: Int) :. _ :. _) = extent arr'
