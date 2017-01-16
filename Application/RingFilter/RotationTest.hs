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
-- import           Graphics.Rendering.Chart.Backend.Cairo
-- import           Graphics.Rendering.Chart.Easy
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

main =
  do (inputPath:pcaFile:degStr:_) <- getArgs
     let parallelParams =
           ParallelParams {numThread = 8
                          ,batchSize = 160}
         filterParamsSet1 =
           PolarSeparableFilterParamsSet {getSizeSet = (0,0)
                                         ,getDownsampleFactorSet = 1
                                         ,getScaleSet =
                                            S.fromDistinctAscList [2]
                                         ,getRadialFreqSet =
                                            S.fromDistinctAscList [0 .. (8 - 1)]
                                         ,getAngularFreqSet =
                                            S.fromDistinctAscList [0 .. (8 - 1)]
                                         ,getNameSet = Pinwheels}
         deg =
           if L.null degStr
              then 90
              else read degStr :: Int
         numTake = 4270
         numDrop = 4260
         idx = 2
     pcaMatrixes <- readMatrixes pcaFile
     labeledImgBuf <-
       readLabeledImagebinarySource inputPath $$
       (do CL.drop numDrop
           ys <- CL.take (numTake - numDrop)
           return ys)
     let !labeledImg = labeledImgBuf
         !img = L.map (\(LabeledArray _ arr) -> arr) labeledImg
         !label = L.map (\(LabeledArray l _) -> l) labeledImg
     -- rotatedLabeledImg <-
     --   CL.sourceList labeledImg $$
     --   rescaleRotateLabeledImageConduit parallelParams
     --                                    299
     --                                    (fromIntegral deg) =$=
     --   CL.consume
     mag <-
       runResourceT $
       (CL.sourceList labeledImg $$
        multiLayerMagnitudeVariedSizedConduit parallelParams
                                              [filterParamsSet1]
                                              1 =$=
        CL.consume)
     pcaMag <-
       runResourceT $
       sourceList mag $$ pcaLabelMultiLayerConduit parallelParams pcaMatrixes =$=
       consume
     let ex@(Z :. nf :. nRows :. numCols) = extent . L.head $ img
         refIdx = 0
         idx90 = 5
         idx90' = 2
         centerIdx = (div nRows 2) * numCols + (div numCols 2)
         img0 = img !! refIdx
         img90 = img !! idx90
         img90' =
           extend (Z :. (1 :: Int) :. All :. All) $
           (rotate90Square2DImageS $
            R.slice img0 (Z :. (0 :: Int) :. All :. All)) !!
           idx90'
         magArrs =
           L.map (pointwiseFeature2Array nRows numCols . L.head . snd) mag
         (Z :. _ :. _ :. magNf) = extent mag0
         mag0 = magArrs !! refIdx
         mag90 = magArrs !! idx90
         mag90' =
           fromListUnboxed (Z :. nRows :. numCols :. magNf) .
           L.concat .
           L.transpose .
           L.map (\i ->
                    R.toList ((rotate90Square2DImageS $
                               R.slice mag0 (Z :. All :. All :. i)) !!
                              idx90')) $
           [0 .. magNf - 1]
         pcaMagArrs =
           L.map (pointwiseFeature2Array nRows numCols . L.head . snd) pcaMag
         pcaMagNf = LA.cols . L.head $ pcaMatrixes
         pcaMag0 = pcaMagArrs !! refIdx
         pcaMag90 = pcaMagArrs !! idx90
         pcaMag90' =
           fromListUnboxed (Z :. nRows :. numCols :. pcaMagNf) .
           L.concat .
           L.transpose .
           L.map (\i ->
                    R.toList ((rotate90Square2DImageS $
                               R.slice pcaMag0 (Z :. All :. All :. i)) !!
                              idx90')) $
           [0 .. pcaMagNf - 1]
     print label
     x1 <- computeP . R.map abs $ (img90 -^ img90')
     let vec = toUnboxed x1
     print . VU.filter (/= 0) $ vec
     print .
       VU.map (\i ->
                 let a = div i 299
                     b = mod i 299
                 in (a,b)) .
       VU.findIndices (/= 0) $
       vec
     print . VU.length . VU.findIndices (/= 0) $ vec
     x2 <-
       sumAllP . R.map abs $
       R.slice (mag90 -^ mag90')
               (Z :. All :. All :. (idx :: Int))
     putStrLn "Avg. difference:"
     print (x2 / (fromIntegral $ nRows * numCols))
     x3 <-
       computeP . R.map abs $
       R.slice (mag90 -^ mag90')
               (Z :. All :. All :. (idx :: Int))
     putStrLn "Max. difference:"
     print . VU.maximum . toUnboxed $ x3
     putStrLn "Max. value"
     x4 <-
       computeP . R.map abs $ R.slice mag90 (Z :. All :. All :. (idx :: Int))
     print . VU.maximum . toUnboxed $ x4
     pcax2 <-
       sumAllP . R.map abs $
       R.slice (pcaMag90 -^ pcaMag90')
               (Z :. All :. All :. (idx :: Int))
     putStrLn "pca Avg. difference:"
     print (pcax2 / (fromIntegral $ nRows * numCols))
     pcax3 <-
       computeP . R.map abs $
       R.slice (pcaMag90 -^ pcaMag90')
               (Z :. All :. All :. (idx :: Int))
     putStrLn "pca Max. difference:"
     print . VU.maximum . toUnboxed $ pcax3
     putStrLn "pca Max. value"
     pcax4 <-
       computeP . R.map abs $
       R.slice pcaMag90 (Z :. All :. All :. (idx :: Int))
     print . VU.maximum . toUnboxed $ pcax4
     plotImage "img0.png" img0
     plotImage "img90.png" img90
     plotImage "img90'.png" $ computeS img90'
     plotImage "mag0.png" .
       computeS .
       R.backpermute (Z :. 1 :. nRows :. numCols)
                     (\(Z :. k :. j :. i) -> (Z :. j :. i :. k)) $
       extend (Z :. All :. All :. (1 :: Int))
              (R.slice mag0 (Z :. All :. All :. (idx :: Int)))
     plotImage "mag90.png" .
       computeS .
       R.backpermute (Z :. 1 :. nRows :. numCols)
                     (\(Z :. k :. j :. i) -> (Z :. j :. i :. k)) $
       extend (Z :. All :. All :. (1 :: Int))
              (R.slice mag90 (Z :. All :. All :. (idx :: Int)))
     plotImage "mag90'.png" .
       computeS .
       R.backpermute (Z :. 1 :. nRows :. numCols)
                     (\(Z :. k :. j :. i) -> (Z :. j :. i :. k)) $
       extend (Z :. All :. All :. (1 :: Int))
              (R.slice mag90' (Z :. All :. All :. (idx :: Int)))
     M.mapM_ (\i ->
                plotImage ("mag_" L.++ show i L.++ ".png") .
                computeS .
                R.backpermute (Z :. 1 :. nRows :. numCols)
                              (\(Z :. k :. j :. i) -> (Z :. j :. i :. k)) $
                extend (Z :. All :. All :. (1 :: Int))
                       (R.slice mag0 (Z :. All :. All :. (i :: Int))))
             [0 .. magNf - 1]
     M.mapM_ (\i ->
                do let featureSlice =
                         R.slice pcaMag0 (Z :. All :. All :. (i :: Int))
                   plotImage ("pcaMag_" L.++ show i L.++ ".png") .
                     computeS .
                     R.backpermute (Z :. 1 :. nRows :. numCols)
                                   (\(Z :. k :. j :. i) -> (Z :. j :. i :. k)) $
                     extend (Z :. All :. All :. (1 :: Int)) featureSlice)
             [0 .. 47]
