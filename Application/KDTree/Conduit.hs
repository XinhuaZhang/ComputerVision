module Application.KDTree.Conduit where

import           Application.KDTree.KDTree
import           Application.KDTree.KdTreeStatic as KDT
import           Classifier.LibSVM
import           Control.DeepSeq
import           Control.Monad                   as M
import           Control.Monad.IO.Class          (liftIO)
import qualified Control.Monad.Parallel          as MP
import           Control.Parallel.Strategies
import           CV.Feature.PolarSeparable
import           CV.Image
import           CV.Utility.Parallel
import           Data.Array                      as IA
import           Data.Array.Unboxed              as AU
import           Data.Binary
import           Data.Conduit
import           Data.Conduit.List               as CL
import           Foreign
import           GHC.Float
import           Prelude                         as P
import           Text.Printf                     (printf)

testConduit :: (NFData a) => ParallelParams -> Conduit a IO a
testConduit parallelParams = do xs <- CL.take (batchSize parallelParams)
                                if P.length xs > 0
                                   then sourceList $!! xs
                                   else return ()

grayImage2FloatArrayConduit :: Conduit GrayImage IO (AU.Array (Int, Int, Int) Float)
grayImage2FloatArrayConduit =
  awaitForever
    (\img ->
        let (nx, ny) = dimensions img
        in yield .
           AU.listArray ((0, 0, 0), (nx - 1, ny - 1, 0)) . P.map double2Float . pixelList $
           img)

grayImage2DoubleArrayConduit :: Conduit GrayImage IO (AU.Array (Int, Int, Int) Double)
grayImage2DoubleArrayConduit =
  awaitForever
    (\img ->
        let (nx, ny) = dimensions img
        in yield . AU.listArray ((0, 0, 0), (nx - 1, ny - 1, 0)) . pixelList $ img)

featurePointSink :: FilePath -> Sink [PolarSeparableFeaturePoint] IO ()
featurePointSink treeFilePath = do
  trees <- consume
  liftIO $ encodeFile treeFilePath trees

libSVMTrainSink
  :: FilePath
  -> ParallelParams
  -> TrainParams
  -> Double
   -> Int -> FilePath
  -> Sink (KdTree Double PolarSeparableFeaturePoint) IO ()
libSVMTrainSink labelPath parallelParams trainParams radius sampleRate treeFilePath = do
  label <- liftIO $ readLabelFile labelPath
  trees <- consume
  let treeIdx =
        P.map (\(PolarSeparableFeaturePoint i j _) -> (i, j)) . KDT.toList . P.head $
        trees
      lb = (\(xs, ys) -> ((P.minimum xs), (P.minimum ys))) . P.unzip $ treeIdx
      ub = (\(xs, ys) -> ((P.maximum xs), (P.maximum ys))) . P.unzip $ treeIdx
      arr =
        parMapChunk
          parallelParams
          rseq
          (IA.array (lb, ub) .
           P.map (\p@(PolarSeparableFeaturePoint i j _) -> ((i, j), p)) . KDT.toList)
          trees
      treeArrPair = P.zip trees arr
      kernel =
        parMapChunk
          parallelParams
          rdeepseq
          (\(x, y) -> similarity x y radius sampleRate) $
        (\xs ->
            withStrategy
              (parListChunk (div (P.length xs) (batchSize parallelParams)) rdeepseq) xs) $
        [ (x, y)
        | x <- treeArrPair
        , y <- treeArrPair ]
  kernelPtr <-
    liftIO $
    MP.sequence $
    P.zipWith getPreComputedKernelFeatureVecPtr [1 ..] $ sp (P.length trees) kernel
  liftIO $ oneVsRestTrain trainParams label kernelPtr
  liftIO $ writeKdTree treeFilePath trees
  where
    sp _len [] = []
    sp len xs = as : sp len bs
      where
        (as, bs) = P.splitAt len xs

libSVMPredictConduit
  :: ParallelParams
  -> [KdTree Double PolarSeparableFeaturePoint]
  -> Double
  -> Int
  -> Conduit (KdTree Double PolarSeparableFeaturePoint) IO (Ptr C'svm_node)
libSVMPredictConduit parallelParams trainTrees radius sampleRate = do
  testTrees <- CL.take (batchSize parallelParams)
  if P.length testTrees > 0
    then do
      let treeIdx1 =
            P.map (\(PolarSeparableFeaturePoint i j _) -> (i, j)) .
            KDT.toList . P.head $
            trainTrees
          lb1 = (\(xs, ys) -> ((P.minimum xs), (P.minimum ys))) . P.unzip $ treeIdx1
          ub1 = (\(xs, ys) -> ((P.maximum xs), (P.maximum ys))) . P.unzip $ treeIdx1
          arr1 =
            parMapChunk
              parallelParams
              rseq
              (IA.array (lb1, ub1) .
               P.map (\p@(PolarSeparableFeaturePoint i j _) -> ((i, j), p)) .
               KDT.toList)
              trainTrees
          treeArrPair1 = P.zip trainTrees arr1
          treeIdx2 =
            P.map (\(PolarSeparableFeaturePoint i j _) -> (i, j)) .
            KDT.toList . P.head $
            testTrees
          lb2 = (\(xs, ys) -> ((P.minimum xs), (P.minimum ys))) . P.unzip $ treeIdx2
          ub2 = (\(xs, ys) -> ((P.maximum xs), (P.maximum ys))) . P.unzip $ treeIdx2
          arr2 =
            parMapChunk
              parallelParams
              rseq
              (IA.array (lb2, ub2) .
               P.map (\p@(PolarSeparableFeaturePoint i j _) -> ((i, j), p)) .
               KDT.toList)
              testTrees
          treeArrPair2 = P.zip testTrees arr2
          pairs =
            (\xs ->
                withStrategy
                  (parListChunk (div (P.length xs) (batchSize parallelParams)) rdeepseq)
                  xs) $
            [ (x, y)
            | x <- treeArrPair2
            , y <- treeArrPair1 ]
          feature =
            parMapChunk
              parallelParams
              rdeepseq
              (\(x, y) -> similarity x y radius sampleRate)
              pairs
      ptrs <-
        liftIO $
        P.mapM (getPreComputedKernelFeatureVecPtr (-1)) $
        sp (P.length treeArrPair1) feature
      sourceList ptrs
      libSVMPredictConduit parallelParams trainTrees radius sampleRate
    else return ()
  where
    sp _len [] = []
    sp len xs = as : sp len bs
      where
        (as, bs) = P.splitAt len xs


testSink :: Sink (KdTree Double PolarSeparableFeaturePoint) IO ()
testSink = do
  xs <- CL.take 2
  if P.length xs > 0
    then do
      let tree1 = P.head xs
          tree2 = P.last xs
          treeSize1 = P.floor . sqrt . P.fromIntegral $ KDT.size tree1
          treeSize2 = P.floor . sqrt . P.fromIntegral$ KDT.size tree2
          arr1 = IA.array ((0,0),(treeSize1 - 1,treeSize1 - 1)) . P.map (\p@(PolarSeparableFeaturePoint i j _) -> ((i,j),p)) . KDT.toList $ tree1
          arr2 = IA.array ((0,0),(treeSize2 - 1,treeSize2 - 1)) . P.map (\p@(PolarSeparableFeaturePoint i j _) -> ((i,j),p)) . KDT.toList $ tree2
          hehe1 = P.replicate 1 (tree1,arr1)
          hehe2 = P.replicate 1 (tree2,arr2)
      liftIO $ print . size $ tree2
      liftIO . print $ KDT.inRadiusCount tree2 1.5 (P.head . KDT.toList $ tree1)
      -- liftIO $ P.mapM_  print $ P.zipWith (\x y -> similarity x y 0.05 11) hehe1 hehe2
      return ()
    else return ()

