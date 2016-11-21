{-# LANGUAGE QuasiQuotes #-}
module CV.Utility.RepaArrayUtility where

import           Data.Array.Repa              as R
import           Data.Array.Repa.Stencil      as R
import           Data.Array.Repa.Stencil.Dim2 as R
import           Data.List                    as L
import           Data.Vector                  as V
import           Data.Vector.Unboxed          as VU
import           Prelude                      as P

-- factor = 2^n, n = 0,1,..
-- the first factor in the list corresponds to the inner-most (right-most) dimension.
downsample
  :: (Source s e
     ,Shape sh)
  => [Int] -> Array s sh e -> Array D sh e
downsample factorList arr
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
  fromFunction
    (shapeOfList dimList)
    (\sh' ->
       let idx = L.zipWith (-) (listOfShape sh') diff
       in if L.or (L.zipWith (\i j -> i < 0 || i >= j) idx dimList)
             then 0
             else arr R.! shapeOfList idx)
  where dimList = L.zipWith max newDims . listOfShape . extent $ arr
        diff =
          L.zipWith (\a b ->
                       if a - b <= 0
                          then 0
                          else div (a - b) 2)
                    newDims
                    dimList


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


bicubicInterpolation
  :: [Array U DIM2 Double] -> (Double,Double) -> Double
bicubicInterpolation ds (y,x)
  | (x < 1) ||
      (x > (fromIntegral nx - 2)) || (y < 1) || (y > (fromIntegral ny - 2)) = 0
  | otherwise = result
  where (Z :. ny :. nx) = extent . P.head $ ds
        x' = x - (fromIntegral . floor $ x)
        y' = y - (fromIntegral . floor $ y)
        idx =
          VU.fromListN
            4
            [(floor y,floor x)
            ,(floor y,ceiling x)
            ,(ceiling y,floor x)
            ,(ceiling y,ceiling x)] :: VU.Vector (Int,Int)
        xs =
          VU.concat .
          P.map (\arr' -> VU.map (\(i,j) -> arr' R.! (Z :. i :. j)) idx) $
          ds
        alpha = V.map (VU.sum . VU.zipWith (*) xs) matrixA
        arr =
          fromListUnboxed (Z :. 4 :. 4) . V.toList $ alpha :: R.Array U DIM2 Double
        arr1 =
          R.traverse arr
                     id
                     (\f idx'@(Z :. j :. i) -> f idx' * (x' ^ i) * (y' ^ j))
        result = sumAllS arr1
        matrixA =
          V.fromListN 16 . P.map (VU.fromListN 16) $
          [[1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
          ,[0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0]
          ,[-3,3,0,0,-2,-1,0,0,0,0,0,0,0,0,0,0]
          ,[2,-2,0,0,1,1,0,0,0,0,0,0,0,0,0,0]
          ,[0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0]
          ,[0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0]
          ,[0,0,0,0,0,0,0,0,-3,3,0,0,-2,-1,0,0]
          ,[0,0,0,0,0,0,0,0,2,-2,0,0,1,1,0,0]
          ,[-3,0,3,0,0,0,0,0,-2,0,-1,0,0,0,0,0]
          ,[0,0,0,0,-3,0,3,0,0,0,0,0,-2,0,-1,0]
          ,[9,-9,-9,9,6,3,-6,-3,6,-6,3,-3,4,2,2,1]
          ,[-6,6,6,-6,-3,-3,3,3,-4,4,-2,2,-2,-2,-1,-1]
          ,[2,0,-2,0,0,0,0,0,1,0,1,0,0,0,0,0]
          ,[0,0,0,0,2,0,-2,0,0,0,0,0,1,0,1,0]
          ,[-6,6,6,-6,-4,-2,4,2,-3,3,-3,3,-2,-1,-2,-1]
          ,[4,-4,-4,4,2,2,-2,-2,2,-2,2,-2,1,1,1,1]]
