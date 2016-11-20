module CV.Utility.RepaArrayUtility where

import           Data.Array.Repa as R
import           Data.List       as L

-- factor = 2^n, n = 0,1,..
-- the first factor in the list corresponds to the inner-most (right-most) dimension.
downsample
  :: (Source s e, Shape sh)
  => [Int] -> Array s sh e -> Array D sh e
downsample factorList arr
  | L.or . L.map (< 1) $ newDList = error "Downsample factors are too large."
  | otherwise =
    backpermute
      (shapeOfList newDList)
      (shapeOfList . L.zipWith (*) factorList . listOfShape)
      arr
  where
    dList = listOfShape . extent $ arr
    newDList = L.zipWith (\x y -> div x y) dList factorList

downsampleUnsafe
  :: (Source s e, Shape sh)
  => [Int] -> Array s sh e -> Array D sh e
downsampleUnsafe factorList arr =
  backpermute newSh (shapeOfList . L.zipWith (*) factorList . listOfShape) arr
  where
    dList = listOfShape $ extent arr
    newSh = shapeOfList $ L.zipWith (\x y -> div x y) dList factorList

crop
  :: (Source s e, Shape sh)
  => [Int] -> [Int] -> Array s sh e -> Array D sh e
crop start len arr
  | (L.or . L.map (< 0) $ start) ||
      (L.or $ L.zipWith3 (\x y z -> x > (z - y)) start len dList) =
    error $
    "Crop out of boundary!\n" L.++ show start L.++ "\n" L.++ show len L.++ "\n" L.++
    show dList
  | otherwise =
    backpermute
      (shapeOfList len)
      (shapeOfList . L.zipWith (+) start . listOfShape)
      arr
  where
    dList = listOfShape $ extent arr

cropUnsafe
  :: (Source s e, Shape sh)
  => [Int] -> [Int] -> Array s sh e -> Array D sh e
cropUnsafe start len arr =
  backpermute
    (shapeOfList len)
    (shapeOfList . L.zipWith (+) start . listOfShape)
    arr
