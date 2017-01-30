{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE QuasiQuotes #-}
module CV.Utility.RepaArrayUtility where

import           Data.Array.Repa              as R
import           Data.Array.Repa.Stencil      as R
import           Data.Array.Repa.Stencil.Dim2 as R
import           Data.List                    as L
import           Data.Vector                  as V
import           Data.Vector.Unboxed          as VU
import           Prelude                      as P
import           Data.Array.CArray            as CA
import           Foreign.Storable

-- factor = 2^n, n = 0,1,..
-- the first factor in the list corresponds to the inner-most (right-most) dimension.
{-# INLINE downsample #-}
downsample
  :: (Source s e
     ,Shape sh)
  => [Int] -> R.Array s sh e -> R.Array D sh e
downsample factorList arr
  | L.any (< 1) newDList = error "Downsample factors are too large."
  | otherwise =
    R.backpermute (shapeOfList newDList)
                (shapeOfList . L.zipWith (*) factorList . listOfShape)
                arr
  where dList = listOfShape . extent $ arr
        newDList = L.zipWith div dList factorList

{-# INLINE downsampleUnsafe #-}
downsampleUnsafe
  :: (Source s e
     ,Shape sh)
  => [Int] -> R.Array s sh e -> R.Array D sh e
downsampleUnsafe factorList arr =
  R.backpermute newSh
              (shapeOfList . L.zipWith (*) factorList . listOfShape)
              arr
  where dList = listOfShape $ extent arr
        newSh = shapeOfList $ L.zipWith div dList factorList
                                        
{-# INLINE crop #-}

crop
  :: (Source s e
     ,Shape sh)
  => [Int] -> [Int] -> R.Array s sh e -> R.Array D sh e
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

{-# INLINE cropUnsafe  #-}

cropUnsafe
  :: (Source s e
     ,Shape sh)
  => [Int] -> [Int] -> R.Array s sh e -> R.Array D sh e
cropUnsafe start len =
  R.backpermute (shapeOfList len)
              (shapeOfList . L.zipWith (+) start . listOfShape)

{-# INLINE pad  #-}

pad :: (Real e
       ,Source s e
       ,Shape sh)
    => [Int] -> R.Array s sh e -> R.Array D sh e
pad newDims arr =
  fromFunction
    (shapeOfList dimList)
    (\sh' ->
       let idx = L.zipWith (-) (listOfShape sh') diff
       in if L.or (L.zipWith (\i j -> i < 0 || i >= j) idx oldDimList)
             then 0
             else arr R.! shapeOfList idx)
  where oldDimList = listOfShape . extent $ arr
        dimList = L.zipWith max newDims oldDimList
        diff =
          L.zipWith (\a b ->
                       if a - b <= 0
                          then 0
                          else div (a - b) 2)
                    newDims
                    oldDimList


computeDerivativeP
  :: R.Array U DIM2 Double -> IO [R.Array U DIM2 Double]
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
  :: R.Array U DIM2 Double -> [R.Array U DIM2 Double]
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
  :: [R.Array U DIM2 Double] -> (Double,Double) ->  (Double,Double) -> Double
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


{-# INLINE twoDCArray2RArray #-}

twoDCArray2RArray
  :: (Num a, Storable a)
  => CArray (Int, Int) a -> R.Array D DIM2 a
twoDCArray2RArray cArr =
  fromFunction
    (Z :. (ny' + 1) :. (nx' + 1))
    (\(Z :. j :. i) -> cArr CA.! (j, i))
  where
    ((_, _), (ny', nx')) = bounds cArr

{-# INLINE threeDRArray2CArray #-}

threeDRArray2CArray
  :: (Num a, Storable a, Source s a)
  => R.Array s DIM3 a -> CArray (Int, Int, Int) a
threeDRArray2CArray rArr =
  listArray ((0, 0, 0), (nf - 1, ny - 1, nx - 1)) . R.toList $ rArr
  where
    (Z :. nf :. ny :. nx) = extent rArr

{-# INLINE threeDCArray2RArray #-}

threeDCArray2RArray
  :: (Num a, Storable a)
  => CArray (Int, Int, Int) a -> R.Array D DIM3 a
threeDCArray2RArray cArr =
  fromFunction
    (Z :. (nf' + 1) :. (ny' + 1) :. (nx' + 1))
    (\(Z :. k :. j :. i) -> cArr CA.! (k, j, i))
  where
    ((_, _, _), (nf', ny', nx')) = bounds cArr


{-# INLINE makeFilterList #-}

makeFilterList :: Int -> Int -> (Int -> Int -> a) -> [a]
makeFilterList ny nx f =
  [ let !x =
          if r < (ny `div` 2)
            then r
            else r - ny
        !y =
          if c < (nx `div` 2)
            then c
            else c - nx
    in f x y
  | r <- [0 .. ny - 1]
  , c <- [0 .. nx - 1] ]


{-# INLINE extractPointwiseFeature #-}

extractPointwiseFeature
  :: (R.Source s Double)
  => R.Array s DIM3 Double -> [VU.Vector Double]
extractPointwiseFeature arr' =
  [ toUnboxed . computeUnboxedS . R.slice arr' $ (Z :. All :. j :. i)
  | j <- [0 .. ny' - 1]
  , i <- [0 .. nx' - 1] ]
  where
    !(Z :. _ :. (ny'::Int) :. (nx'::Int)) = extent arr'
    
{-# INLINE extractFeatureMap #-}

extractFeatureMap
  :: (R.Source s Double)
  => R.Array s DIM3 Double -> [[Double]]
extractFeatureMap arr' =
  L.map (\k -> R.toList . R.slice arr' $ (Z :. k :. All :. All)) [0 .. nf' - 1]
  where
    !(Z :. (nf' :: Int) :. _ :. _) = extent arr'
