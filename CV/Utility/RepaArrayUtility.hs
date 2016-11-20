module CV.Utility.RepaArrayUtility where

import Data.Array.Repa as R
import Data.List as L

-- factor = 2^n, n = 0,1,..
-- the first factor in the list corresponds to the inner-most (right-most) dimension.
downsample
  :: (Source s e, Shape sh)
  => Array s sh e -> [Int] -> Array D sh e
downsample arr factorList
  | (L.length . L.findIndices (< 1) $ newDList) > 0 =
    error "Downsample factors are too large."
  | otherwise =
    backpermute
      (shapeOfList newDList)
      (\sh -> shapeOfList . L.zipWith (*) factorList . listOfShape $ sh)
      arr
  where
    dList = listOfShape . extent $ arr
    newDList = L.zipWith (\x y -> div x y) dList factorList
    
downsampleUnsafe
  :: (Source s e, Shape sh)
  => Array s sh e -> [Int] -> Array D sh e
downsampleUnsafe arr factorList =
  backpermute
    newDList
    (\sh -> shapeOfList . L.zipWith (*) factorList . listOfShape $ sh)
    arr
  where
    dList = listOfShape $ extent arr
    newDList = shapeOfList $ L.zipWith (\x y -> div x y) dList factorList
