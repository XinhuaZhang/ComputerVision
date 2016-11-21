module CV.Utility.RepaArrayUtility where

import           Data.Array.Repa as R
import           Data.List       as L

-- factor = 2^n, n = 0,1,..
-- the first factor in the list corresponds to the inner-most (right-most) dimension.
downsample
  :: (Source s e
     ,Shape sh)
  => [Int] -> Array s sh e -> Array D sh e
downsample factorList arr
  | L.any (< 1) newDList = error "Downsample factors are too large."
  | otherwise =
    backpermute (shapeOfList newDList)
                (shapeOfList . L.zipWith (*) factorList . listOfShape)
                arr
  where dList = listOfShape . extent $ arr
        newDList = L.zipWith div dList factorList

downsampleUnsafe
  :: (Source s e
     ,Shape sh)
  => [Int] -> Array s sh e -> Array D sh e
downsampleUnsafe factorList arr =
  backpermute newSh
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
    backpermute (shapeOfList len)
                (shapeOfList . L.zipWith (+) start . listOfShape)
                arr
  where dList = listOfShape $ extent arr

cropUnsafe
  :: (Source s e
     ,Shape sh)
  => [Int] -> [Int] -> Array s sh e -> Array D sh e
cropUnsafe start len =
  backpermute (shapeOfList len)
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
             else arr ! shapeOfList idx)
  where dimList = L.zipWith max newDims . listOfShape . extent $ arr
        diff =
          L.zipWith (\a b ->
                       if a - b <= 0
                          then 0
                          else div (a - b) 2)
                    newDims
                    dimList
