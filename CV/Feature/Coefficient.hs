{-# LANGUAGE FlexibleContexts #-}
module CV.Feature.Coefficient where

import           Control.Monad                as M
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           CV.Utility.Parallel
import           CV.Utility.RepaArrayUtility
import           Data.Array.CArray            as CA
import           Data.Array.Repa              as R
import           Data.Complex
import           Data.Conduit
import           Data.Conduit.List            as CL
import           Data.List                    as L
import           Math.FFT
import           System.Random

type ArrayChannels = [R.Array U DIM2 (Complex Double)]

type Coefficient = [CArray (Int, Int, Int) (Complex Double)]

type Filter = R.Array U DIM3 (Complex Double)                    -- Fourier transformed

type FlippedFilter = CArray (Int, Int, Int) (Complex Double)     -- Flipped then Fourier transformed

{-# INLINE reconstruction #-}

reconstruction :: FlippedFilter -> Coefficient -> ArrayChannels
reconstruction filter' =
  L.map
    (\c ->
        let ((_, _, _), (nf', ny', nx')) = bounds filter'
            filteredCArr =
              idftN [1, 2] . liftArray2 (*) filter' . dftN [1, 2] $ c
        in sumS $
           fromFunction
             (Z :. ny' + 1 :. nx' + 1 :. nf' + 1)
             (\(Z :. j :. i :. k) -> filteredCArr CA.! (k, j, i)))


{-# INLINE imageArray2ArrayChannels #-}

imageArray2ArrayChannels
  :: (R.Source s Double)
  => R.Array s DIM3 Double -> ArrayChannels
imageArray2ArrayChannels arr =
  L.map
    (\i -> computeS . R.slice complexArr $ (Z :. i :. All :. All))
    [0 .. nf' - 1]
  where
    complexArr = R.map (:+ 0) arr
    (Z :. nf' :. _ny' :. _nx') = extent arr

{-# INLINE convolve #-}

convolve :: Filter -> ArrayChannels -> Coefficient
convolve filter' =
  L.map
    (\img ->
        let dftImg = twoDCArray2RArray . dftN [0, 1] . twoDRArray2CArray $ img
            filteredArr =
              R.traverse2 filter' dftImg const $
              \fFilter fImg idx@(Z :. _ :. j :. i) ->
                 fFilter idx * fImg (Z :. j :. i)
        in idftN [1, 2] . threeDRArray2CArray $ filteredArr)

{-# INLINE gradientDecent #-}

gradientDecent
  :: Double
  -> ArrayChannels
  -> Filter
  -> FlippedFilter
  -> Coefficient
  -> Double
  -> Coefficient
gradientDecent learningRate inputs filter' flippedFilter' coefficient lastEnergy
  | energy >= lastEnergy = coefficient
  | otherwise =
    gradientDecent learningRate inputs filter' flippedFilter' newCoefficient energy
  where
    recon = reconstruction flippedFilter' coefficient
    errors = L.map computeS $ L.zipWith (-^) inputs recon
    l2Error = L.sum . L.map (sumAllS . R.map (^ (2 :: Int))) $ errors
    energy = magnitude l2Error
    delta = convolve filter' errors
    newCoefficient =
      L.zipWith (liftArray2 (\a d -> a + (learningRate :+ 0) * d)) coefficient delta

{-# INLINE gradientDecentIO #-}

gradientDecentIO
  :: Double
  -> ArrayChannels
  -> Filter
  -> FlippedFilter
  -> Coefficient
  -> Double
  -> IO Coefficient
gradientDecentIO learningRate inputs filter' flippedFilter' coefficient lastEnergy
  | energy >= lastEnergy = do
    putStrLn ""
    return coefficient
  | otherwise = do
    print energy
    gradientDecentIO learningRate inputs filter' flippedFilter' newCoefficient energy
  where
    recon = reconstruction flippedFilter' coefficient
    errors = L.map computeS $ L.zipWith (-^) inputs recon
    l2Error = L.sum . L.map (sumAllS . R.map (^ (2 :: Int))) $ errors
    energy = magnitude l2Error
    delta = convolve filter' errors
    newCoefficient =
      L.zipWith (liftArray2 (\a d -> a + (learningRate :+ 0) * d)) coefficient delta

{-# INLINE generateComplexNumber #-}

generateComplexNumber :: (Double,Double) -> IO (Complex Double)
generateComplexNumber bound = do
  a <- randomRIO bound
  b <- randomRIO bound
  return (a :+ b)
  

computeCoefficient
  :: (R.Source s Double)
  => Double
  -> Filter
  -> FlippedFilter
  -> R.Array s DIM3 Double
  -> Coefficient
  -> R.Array U DIM3 (Complex Double)
computeCoefficient learningRate filter' flippedFilter' img initCoefficients =
  fromListUnboxed (Z :. imgNf * filterNf :. imgNy :. imgNx) . L.concatMap elems $
  gradientDecent
    learningRate
    (imageArray2ArrayChannels img)
    filter'
    flippedFilter'
    initCoefficients
    (fromIntegral (maxBound :: Int))
  where
    (Z :. imgNf :. imgNy :. imgNx) = extent img
    (Z :. filterNf :. _ :. _) = extent filter'

computeCoefficientIO
  :: (R.Source s Double)
  => Double
  -> Filter
  -> FlippedFilter
  -> R.Array s DIM3 Double
  -> IO (R.Array U DIM3 (Complex Double))
computeCoefficientIO learningRate filter' flippedFilter' img = do
  initCoefficients <-
    M.replicateM imgNf .
    fmap (listArray ((0, 0, 0), (filterNf - 1, imgNy - 1, imgNx - 1))) .
    M.replicateM (filterNf * imgNy * imgNx) . generateComplexNumber $
    (0.01, 0.01)
  coefficients <-
    gradientDecentIO
      learningRate
      (imageArray2ArrayChannels img)
      filter'
      flippedFilter'
      initCoefficients
      (fromIntegral (maxBound :: Int))
  return .
    fromListUnboxed (Z :. imgNf * filterNf :. imgNy :. imgNx) . L.concatMap elems $
    coefficients
  where
    (Z :. imgNf :. imgNy :. imgNx) = extent img
    (Z :. filterNf :. _ :. _) = extent filter'


coefficientMagnitudeFixedSizeConduitIO
  :: (R.Source s Double)
  => Double
  -> Filter
  -> FlippedFilter
  -> Conduit (R.Array s DIM3 Double) (ResourceT IO) (R.Array U DIM3 Double)
coefficientMagnitudeFixedSizeConduitIO learningRate filter' flippedFilter' =
  awaitForever
    (\x -> do
       arr <-
         liftIO $ computeCoefficientIO learningRate filter' flippedFilter' x
       yield . computeS . R.map magnitude $ arr)


coefficientMagnitudeConduit
  :: (R.Source s Double)
  => ParallelParams
  -> Double
  -> Filter
  -> FlippedFilter
  -> Conduit (R.Array s DIM3 Double) IO (R.Array U DIM3 Double)
coefficientMagnitudeConduit parallelParams learningRate filter' flippedFilter' = do
  xs <- CL.take (batchSize parallelParams)
  unless
    (L.null xs)
    (do let (Z :. imgNf :. imgNy :. imgNx) = extent . L.head $ xs
            (Z :. filterNf :. _ :. _) = extent filter'
        initCoefficients <-
          liftIO .
          M.replicateM (L.length xs) .
          M.replicateM imgNf .
          fmap (listArray ((0, 0, 0), (filterNf - 1, imgNy - 1, imgNx - 1))) .
          M.replicateM (filterNf * imgNy * imgNx) . generateComplexNumber $
          (0.01, 0.01)
        let ys =
              parZipWithChunk
                parallelParams
                rseq
                (\x c ->
                    let arr =
                          computeS .
                          R.map magnitude .
                          computeCoefficient
                            learningRate
                            filter'
                            flippedFilter'
                            x $
                          c
                    in deepSeqArray arr arr)
                xs
                initCoefficients
        sourceList ys
        coefficientMagnitudeConduit parallelParams learningRate filter' flippedFilter')
