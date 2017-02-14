{-# LANGUAGE FlexibleContexts #-}
module Application.Pinwheel.Reconstruction where

import           Control.Monad                  as M
import           CV.Filter.PolarSeparableFilter
import           CV.Utility.RepaArrayUtility
import           CV.Utility.Parallel
import           Data.Array                     as Arr
import           Data.Array.CArray              as CA
import           Data.Array.Repa                as R
import           Data.Complex                   as C
import qualified Data.Image                     as UNM
import           Data.List                      as L
import           Data.Vector.Unboxed            as VU
import           System.Random
import           Math.FFT
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
  :: (R.Source s (Complex Double), R.Source s1 Double)
  => Double
  -> Int
  -> [R.Array s DIM2 (Complex Double)]
  -> R.Array s1 DIM3 Double
  -> IO [[Complex Double]]
computeActivityComplex learningRate count filters img
  | imageNy /= filterNy || imageNx /= filterNx =
    error
      "computeActivityComplex: the image and the filter have different sizes."
  | otherwise = do
    act <- M.replicateM (L.length filters * imageNf) $ generateComplexNumber (-1,1)
    gradientDecentComplex
      count
      learningRate
      (L.map (toUnboxed . computeS . R.map (:+ 0)) imageList)
      (L.map (toUnboxed. computeS . delay) filters)
      (splitList (L.length filters) act)
  where
    (Z :. imageNf :. imageNy :. imageNx) = extent img
    imageList =
      L.map
        (\i -> R.slice img (Z :. (i :: Int) :. All :. All))
        [0 .. imageNf - 1]
    (Z :. filterNy :. filterNx) = extent . L.head $ filters
    

computeActivityConvolution
  :: (R.Source s (Complex Double), R.Source s1 Double)
  => Double
  -> Int
  -> [R.Array s DIM2 (Complex Double)]
  -> [R.Array s DIM2 (Complex Double)]
  -> R.Array s1 DIM3 Double
  -> IO [[R.Array U DIM2 (Complex Double)]]
computeActivityConvolution learningRate count filters filters180 img
  | imageNy /= filterNy || imageNx /= filterNx =
    error
      "computeActivityComplex: the image and the filter have different sizes."
  | otherwise = do
    act <-
      M.replicateM (L.length filters * imageNf * filterNx * filterNy) $
      generateComplexNumber (-1, 1)
    gradientDecentConvolution
      count
      learningRate
      (L.map (R.map (:+ 0)) imageList)
      (L.map (computeS . delay) filters)
      (L.map (computeS . delay) filters180)
      (L.map
         (L.map (fromListUnboxed (Z :. filterNy :. filterNx)) .
          splitList (filterNx * filterNy)) .
       splitList (L.length filters * filterNx * filterNy) $
       act)
  where
    (Z :. imageNf :. imageNy :. imageNx) = extent img
    imageList =
      L.map
        (\i -> R.slice img (Z :. (i :: Int) :. All :. All))
        [0 .. imageNf - 1]
    (Z :. filterNy :. filterNx) = extent . L.head $ filters


computeReconComplex
  :: (R.Source s (Complex Double))
  => [R.Array s DIM2 (Complex Double)] -> [[Complex Double]] -> R.Array U DIM3 (Complex Double)
computeReconComplex filters acts =
  fromUnboxed (Z :. L.length acts :. filterNy :. filterNx) .
  VU.concat .
  L.map
    (L.foldl1' (VU.zipWith (+)) .
     L.zipWith (\arr' a -> toUnboxed . computeS . R.map (* a) $ arr') filters) $
  acts
  where
    (Z :. filterNy :. filterNx) = extent . L.head $ filters
    

{-# INLINE computeReconConvolution #-}

computeReconConvolution
  :: (R.Source s (Complex Double))
  => [R.Array s DIM2 (Complex Double)]
  -> [[R.Array s DIM2 (Complex Double)]]
  -> [R.Array U DIM2 (Complex Double)]
computeReconConvolution filters =
  L.map
    (sumS .
     L.foldl1' R.append .
     parMap rseq (extend (Z :. All :. All :. (1 :: Int)) . uncurry convolve2D) .
     L.zip filters)


plotComplexImage
  :: (R.Source s (Complex Double))
  => String -> R.Array s DIM3 (Complex Double) -> IO ()
plotComplexImage prefix arr = do
  M.zipWithM_
    (\i arr' ->
        UNM.writeImage
          (prefix L.++ "_complex_" L.++ show i L.++
           ".ppm")
          arr')
    [1 .. imageNf]
    arrList
  M.zipWithM_
    (\i arr' ->
        UNM.writeImage
          (prefix L.++ "_magnitude_" L.++ show i L.++
           ".pgm") $
        (UNM.realPart arr' :: UNM.GrayImage))
    [1 .. imageNf]
    arrList
  where
    (Z :. imageNf :. imageNy :. imageNx) = extent arr
    arrList =
      L.map
        (\i ->
            UNM.arrayToImage .
            Arr.listArray ((0, 0), (imageNy - 1, imageNx - 1)) . R.toList $
            R.slice arr (Z :. i :. All :. All) :: UNM.ComplexImage)
        [0 .. imageNf - 1]
        

plotComplexImages
  :: (R.Source s (Complex Double))
  => String -> [R.Array s DIM2 (Complex Double)] -> IO ()
plotComplexImages prefix arrs = do
  M.zipWithM_
    (\i arr' ->
        UNM.writeImage (prefix L.++ "_complex_" L.++ show i L.++ ".ppm") arr')
    [1 .. imageNf]
    arrList
  M.zipWithM_
    (\i arr' ->
        UNM.writeImage (prefix L.++ "_magnitude_" L.++ show i L.++ ".pgm") $
        (UNM.realPart arr' :: UNM.GrayImage))
    [1 .. imageNf]
    arrList
  where
    (Z :. imageNy :. imageNx) = extent . L.head $ arrs
    imageNf = L.length arrs
    arrList =
      L.map
        (\arr' ->
            UNM.arrayToImage .
            Arr.listArray ((0, 0), (imageNy - 1, imageNx - 1)) . R.toList $
            arr')
        arrs

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
    print activities
    return activities
  | otherwise
   -- print (sqrt . VU.sum . VU.map (^ (2 :: Int)) $ error')
   = do
    print (sqrt . VU.sum . VU.map (^ (2 :: Int)) . VU.concat $ errors)
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


{-# INLINE gradientDecentComplex #-}

gradientDecentComplex
  :: Int
  -> Double
  -> [VU.Vector (Complex Double)] -- multi-channel image
  -> [VU.Vector (Complex Double)]
  -> [[Complex Double]]
  -> IO [[Complex Double]]
gradientDecentComplex count learningRate imgs basis activities
  | count == 0 = do
    print (VU.sum . VU.map (^ (2 :: Int)) . VU.concat $ errors)
    return activities
  | otherwise
   = do
    print . VU.sum . VU.map (^ (2 :: Int)) . VU.concat $ errors
    gradientDecentComplex
      (count - 1)
      learningRate
      imgs
      basis
      (L.zipWith (L.zipWith (\a d -> a + (learningRate :+ 0) * d)) activities delta)
  where
    errors =
      L.zipWith
        (\img act ->
            VU.zipWith (-) img . L.foldl1' (VU.zipWith (+)) $
            L.zipWith (\p a -> VU.map (* a) p) basis act)
        imgs
        activities
    delta = L.map (\error' -> L.map (VU.sum . VU.zipWith (*) error') basis) errors
    
gradientDecentConvolution
  :: (R.Source s (Complex Double))
  => Int
  -> Double
  -> [R.Array s DIM2 (Complex Double)]
  -> [R.Array U DIM2 (Complex Double)]
  -> [R.Array U DIM2 (Complex Double)]
  -> [[R.Array U DIM2 (Complex Double)]]
  -> IO [[R.Array U DIM2 (Complex Double)]]
gradientDecentConvolution count learningRate imgs basis basis180 activities
  | count == 0 = return activities
  | otherwise = do
    e <- M.mapM (sumAllP . R.map (^ (2 :: Int))) errors
    print e
    gradientDecentConvolution count learningRate imgs basis basis180 newAct
  where
    errors = L.zipWith (-^) imgs . computeReconConvolution basis180 $ activities
    delta = L.map (\error' -> parMap rseq (convolve2D error') basis) errors
    newAct =
      L.zipWith
        (L.zipWith (\a d -> computeS $ a +^ R.map (* (learningRate :+ 0)) d))
        activities
        delta



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

{-# INLINE generateComplexNumber #-}

generateComplexNumber :: (Double,Double) -> IO (Complex Double)
generateComplexNumber bound = do
  a <- randomRIO bound
  b <- randomRIO bound
  return (a :+ b)

{-# INLINE complexVec2RealVec #-}

complexVec2RealVec :: VU.Vector (Complex Double) -> VU.Vector Double
complexVec2RealVec = VU.fromList . L.concatMap (\(a :+ b) -> [a,b]) . VU.toList


{-# INLINE convolve2D #-}

convolve2D
  :: (R.Source s1 (Complex Double), R.Source s2 (Complex Double))
  => R.Array s1 DIM2 (Complex Double)
  -> R.Array s2 DIM2 (Complex Double)
  -> R.Array D DIM2 (Complex Double)
convolve2D arr1 arr2 =
  twoDCArray2RArray . idftN [0, 1] $ liftArray2 (*) dftCArr1 dftCArr2
  where
    dftCArr1 = dftN [0, 1] . twoDRArray2CArray $ arr1
    dftCArr2 = dftN [0, 1] . twoDRArray2CArray $ arr2
