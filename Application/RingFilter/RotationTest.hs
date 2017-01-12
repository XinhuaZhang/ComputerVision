{-# LANGUAGE BangPatterns #-}

module Main where

import           Application.GMM.PCA
import           Application.RotateDataset.RotationRepa
import           Control.Monad                          as M
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           CV.Array.Image
import           CV.Array.LabeledArray
import           CV.Feature.PolarSeparable
import           CV.Filter.PolarSeparableFilter
import           CV.IO.ImageIO
import           CV.Utility.Parallel
import           Data.Array.Repa                        as R
import           Data.Conduit                           as C
import           Data.Conduit.List                      as CL
import           Data.Conduit.Binary                      as CB
import           Data.List                              as L
import           Data.Set                               as S
import           Data.Vector.Unboxed                    as VU
import           Graphics.Rendering.Chart.Backend.Cairo
import           Graphics.Rendering.Chart.Easy
import           System.Environment
import qualified Numeric.LinearAlgebra.Data     as LA

readLabelFile :: FilePath -> IO [Int]
readLabelFile filePath = do
  bs <- readFile filePath
  return . L.map (\x -> read x :: Int) . L.lines $! bs

labelSource :: FilePath -> C.Source IO Int
labelSource filePath =
  do labels <- liftIO . readLabelFile $ filePath
     sourceList labels
     

pointwiseFeature2Array :: Int
                       -> Int
                       -> [VU.Vector Double]
                       -> R.Array U DIM3 Double
pointwiseFeature2Array nr nc vecs
  | nc * nr == L.length vecs =
    fromUnboxed (Z :. nr :. nc :. (VU.length . L.head $ vecs)) . VU.concat $
    vecs
  | otherwise = error "pointwiseFeature2Array: dimension mismatched."

main = do
  (inputPath:pcaFile:degStr:_) <- getArgs
  let parallelParams = ParallelParams {numThread = 8, batchSize = 160}
      filterParamsSet1 =
        PolarSeparableFilterParamsSet
        { getSizeSet = (0, 0)
        , getDownsampleFactorSet = 1
        , getScaleSet = S.fromDistinctAscList [4]
        , getRadialFreqSet = S.fromDistinctAscList [0 .. (8 - 1)]
        , getAngularFreqSet = S.fromDistinctAscList [0 .. (8 - 1)]
        , getNameSet = Pinwheels
        }
      deg =
        if L.null degStr
          then 90
          else read degStr :: Int
      numTake = 16
      numDrop = 12
      idx = 1
  pcaMatrixes <- readMatrixes pcaFile
  labeledImgBuf <- readLabeledImageBinary inputPath numTake
  let !labeledImg = L.drop numDrop labeledImgBuf
      !img = L.map (\(LabeledArray _ arr) -> arr) labeledImg
  rotatedLabeledImg <-
    CL.sourceList labeledImg $$
    rescaleRotateLabeledImageConduit parallelParams 299 (fromIntegral deg) =$=
    CL.consume
  mag <-
    runResourceT $
    (CL.sourceList rotatedLabeledImg $$
     multiLayerMagnitudeVariedSizedConduit parallelParams [filterParamsSet1] 1 =$=
     CL.consume)
  pcaMag <-
    runResourceT $
    sourceList mag $$ pcaLabelMultiLayerConduit parallelParams pcaMatrixes =$=
    consume
  let ex@(Z :. nf :. nRows :. numCols) = extent . L.head $ img
      centerIdx = (div nRows 2) * numCols + (div numCols 2)
      img0 = img !! 0
      img90 = img !! 1
      img90' =
        extend (Z :. (1 :: Int) :. All :. All) $
        (rotate90Square2DImageS $ R.slice img0 (Z :. (0 :: Int) :. All :. All)) !!
        1
      magArrs = L.map (pointwiseFeature2Array nRows numCols . L.head . snd) mag
      (Z :. _ :. _ :. magNf) = extent mag0
      mag0 = magArrs !! 0
      mag90 = magArrs !! 1
      mag90' =
        fromListUnboxed (Z :. nRows :. numCols :. magNf) .
        L.concat .
        L.transpose .
        L.map
          (\i ->
             R.toList
               ((rotate90Square2DImageS $ R.slice mag0 (Z :. All :. All :. i)) !!
                1)) $
        [0 .. magNf - 1]
      pcaMagArrs =
        L.map (pointwiseFeature2Array nRows numCols . L.head . snd) pcaMag
      pcaMagNf = LA.cols . L.head $ pcaMatrixes
      pcaMag0 = pcaMagArrs !! 0
      pcaMag90 = pcaMagArrs !! 1
      pcaMag90' =
        fromListUnboxed (Z :. nRows :. numCols :. pcaMagNf) .
        L.concat .
        L.transpose .
        L.map
          (\i ->
             R.toList
               ((rotate90Square2DImageS $ R.slice pcaMag0 (Z :. All :. All :. i)) !!
                1)) $
        [0 .. pcaMagNf - 1]
  -- print . extent $ mag0
  -- print . extent $ mag90
  -- print . extent $ mag90'
  x1 <- sumAllP . R.map abs $ (img90 -^ img90')
  print x1
  x2 <-
    sumAllP . R.map abs $
    R.slice (mag90 -^ mag90') (Z :. All :. All :. (idx :: Int))
  putStrLn "Avg. difference:"
  print (x2 / (fromIntegral $ nRows * numCols))
  x3 <-
    computeP . R.map abs $
    R.slice (mag90 -^ mag90') (Z :. All :. All :. (idx :: Int))
  putStrLn "Max. difference:"
  print . VU.maximum . toUnboxed $ x3
  putStrLn "Max. value"
  x4 <- computeP . R.map abs $ R.slice mag90 (Z :. All :. All :. (idx :: Int))
  print . VU.maximum . toUnboxed $ x4
  pcax2 <-
    sumAllP . R.map abs $
    R.slice (pcaMag90 -^ pcaMag90') (Z :. All :. All :. (idx :: Int))
  putStrLn "pca Avg. difference:"
  print (pcax2 / (fromIntegral $ nRows * numCols))
  pcax3 <-
    computeP . R.map abs $
    R.slice (pcaMag90 -^ pcaMag90') (Z :. All :. All :. (idx :: Int))
  putStrLn "pca Max. difference:"
  print . VU.maximum . toUnboxed $ pcax3
  putStrLn "pca Max. value"
  pcax4 <-
    computeP . R.map abs $ R.slice pcaMag90 (Z :. All :. All :. (idx :: Int))
  print . VU.maximum . toUnboxed $ pcax4
  plotImage "img0.png" img0
  plotImage "img90.png" img90
  plotImage "mag0.png" .
    computeS .
    R.backpermute
      (Z :. 1 :. nRows :. numCols)
      (\(Z :. k :. j :. i) -> (Z :. j :. i :. k)) $
    extend
      (Z :. All :. All :. (1 :: Int))
      (R.slice mag0 (Z :. All :. All :. (idx :: Int)))
  plotImage "mag90.png" .
    computeS .
    R.backpermute
      (Z :. 1 :. nRows :. numCols)
      (\(Z :. k :. j :. i) -> (Z :. j :. i :. k)) $
    extend
      (Z :. All :. All :. (1 :: Int))
      (R.slice mag90 (Z :. All :. All :. (idx :: Int)))
  plotImage "mag90'.png" .
    computeS .
    R.backpermute
      (Z :. 1 :. nRows :. numCols)
      (\(Z :. k :. j :. i) -> (Z :. j :. i :. k)) $
    extend
      (Z :. All :. All :. (1 :: Int))
      (R.slice mag90' (Z :. All :. All :. (idx :: Int)))
  M.mapM_
    (\i -> do
       let featureSlice = R.slice pcaMag0 (Z :. All :. All :. (idx :: Int))
       print . VU.maximum . toUnboxed . computeS $ featureSlice
       plotImage ("pcaMag" L.++ show i L.++ ".png") .
         computeS .
         R.backpermute
           (Z :. 1 :. nRows :. numCols)
           (\(Z :. k :. j :. i) -> (Z :. j :. i :. k)) $
         extend (Z :. All :. All :. (1 :: Int)) featureSlice)
    [0 .. 47]
