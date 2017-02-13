{-# LANGUAGE FlexibleContexts #-}
module Application.Pinwheel.Reconstruction where

import           Control.Monad               as M
import           CV.Utility.RepaArrayUtility
import           Data.Array.Repa             as R
import           Data.List                   as L
import           Data.Vector.Unboxed         as VU
import           System.Random
import Data.Complex as C
import qualified Data.Image as UNM
import Data.Array as Arr
import           CV.Filter.PolarSeparableFilter

-- computeActivity
--   :: (R.Source s Double)
--   => Double -> Int -> R.Array s DIM3 Double -> R.Array U DIM3 Double -> IO [Double]
-- computeActivity learningRate count filteredArr img = do
--   act <- M.replicateM filterNf $ randomRIO (-1, 1)
--   gradientDecent count learningRate (toUnboxed img) featureMapList act
--   where
--     (Z :. imageNf :. _ :. _) = extent img
--     featureMapList =
--       L.map (VU.fromList . L.concat) . splitList imageNf . extractFeatureMap $
--       filteredArr
--     filterNf = L.length featureMapList

computeActivityComplex
  :: (R.Source s (Complex Double))
  => Double -> Int -> [R.Array s DIM2 (Complex Double)] -> R.Array U DIM3 Double -> IO [[Double]]
computeActivityComplex learningRate count filters img
  | imageNy /= filterNy || imageNx /= filterNx =
    error
      "computeActivityComplex: the image and the filter have different sizes."
  | otherwise = do
    act <- M.replicateM (L.length filters * imageNf) $ randomRIO (-1, 1)
    gradientDecent
      count
      learningRate
      (L.map (complexArr2RealVec . R.map (:+ 0)) imageList)
      (L.map (complexArr2RealVec . delay) filters)
      (splitList imageNf act)
  where
    (Z :. imageNf :. imageNy :. imageNx) = extent img
    imageList =
      L.map
        (\i -> R.slice img (Z :. (i :: Int) :. All :. All))
        [0 .. imageNf - 1]
    (Z :. filterNy :. filterNx) = extent . L.head $ filters
    complexArr2RealVec = VU.fromList . L.concatMap (\(a :+ b) -> [a, b]) . R.toList 

computeReconComplex
  :: (R.Source s (Complex Double))
  => [R.Array s DIM2 (Complex Double)] -> [[Double]] -> R.Array U DIM3 (Complex Double)
computeReconComplex filters acts =
  fromUnboxed (Z :. L.length acts :. filterNy :. filterNx) .
  VU.concat .
  L.map
    (L.foldl1' (VU.zipWith (+)) .
     L.zipWith (\arr' a -> toUnboxed . computeS . R.map (* a) $ arr') filters .
     L.map (:+ 0)) $
  acts
  where
    (Z :. filterNy :. filterNx) = extent . L.head $ filters
    

plotComplexImage
  :: (R.Source s (Complex Double))
  => FilePath -> R.Array s DIM3 (Complex Double) -> IO ()
plotComplexImage folderPath arr =
  M.zipWithM_
    (\i arr' ->
        UNM.writeImage (folderPath L.++ "/complex_" L.++ show i L.++ ".ppm") arr')
    [1 .. imageNf]
    arrList
  where
    (Z :. imageNf :. imageNy :. imageNx) = extent arr
    arrList =
      L.map
        (\i ->
            UNM.arrayToImage .
            listArray ((0, 0), (imageNy - 1, imageNx - 1)) . R.toList $
            R.slice arr (Z :. i :. All :. All) :: UNM.ComplexImage)
        [0 .. imageNf - 1]

{-# INLINE gradientDecent #-}

gradientDecent
  :: Int
  -> Double
  -> [VU.Vector Double] -- multi-channel image
  -> [VU.Vector Double]
  -> [[Double]]
  -> IO [[Double]]
gradientDecent count learningRate imgs basis activities
  | count == 0 = do
    print (sqrt . VU.sum . VU.map (^ (2 :: Int)) . VU.concat $ errors)
    return activities
  | otherwise
   -- print (sqrt . VU.sum . VU.map (^ (2 :: Int)) $ error')
   = do
    gradientDecent
      (count - 1)
      learningRate
      imgs
      basis
      (L.zipWith (L.zipWith (\a d -> a + learningRate * d)) activities delta)
  where
    errors =
      L.zipWith
        (\img act ->
            VU.zipWith (-) img . L.foldl1' (VU.zipWith (+)) $
            L.zipWith (\p a -> VU.map (* a) p) basis act)
        imgs
        activities
    delta = L.map (\error' -> L.map (VU.sum . VU.zipWith (*) error') basis) errors

{-# INLINE splitList #-}

splitList :: Int -> [a] -> [[a]]
splitList _ [] = []
splitList n xs = as : splitList n bs
  where
    (as, bs) = L.splitAt n xs


{-# INLINE generateComplexFilters #-}

generateComplexFilters :: PolarSeparableFilterParamsSet -> [R.Array U DIM2 (Complex Double)]
generateComplexFilters =
  L.map
    (\params@(PolarSeparableFilterParams (nx', ny') _downSampleFactor scale rf af _name) ->
        let centerY = div ny' 2
            centerX = div nx' 2
        in fromListUnboxed
             (Z :. ny' :. nx')
             [ getFilterFunc params scale rf af (x - centerX) (y - centerY)
             | x <- [0 .. nx' - 1]
             , y <- [0 .. ny' - 1] ]) .
  generatePSFParamsSet
