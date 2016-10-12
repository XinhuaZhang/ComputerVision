module Application.KDTree.Conduit where

import           Application.KDTree.KDTree
import           Classifier.LibSVM
import           Control.DeepSeq
import           Control.Monad             as M
import           Control.Monad.IO.Class    (liftIO)
import qualified Control.Monad.Parallel    as MP
import           CV.Feature.PolarSeparable
import           CV.Image
import           CV.Utility.Parallel
import           Data.Array.Unboxed        as AU
import           Data.Conduit
import           Data.Conduit.List         as CL
import           Data.KdTree.Static        as KDT
import           Foreign
import           GHC.Float
import           Prelude                   as P
import           Text.Printf               (printf)

grayImage2FloatArrayConduit :: Conduit GrayImage IO (AU.Array (Int, Int, Int) Float)
grayImage2FloatArrayConduit =
  awaitForever
    (\img ->
        let (nx, ny) = dimensions img
        in yield .
           listArray ((0, 0, 0), (nx - 1, ny - 1, 0)) . P.map double2Float . pixelList $
           img)

grayImage2DoubleArrayConduit :: Conduit GrayImage IO (AU.Array (Int, Int, Int) Double)
grayImage2DoubleArrayConduit =
  awaitForever
    (\img ->
        let (nx, ny) = dimensions img
        in yield . listArray ((0, 0, 0), (nx - 1, ny - 1, 0)) . pixelList $ img)


libSVMTrainSink
  :: FilePath
  -> ParallelParams
  -> TrainParams
  -> Double
  -> Sink (KdTree Double PolarSeparableFeaturePoint) IO [KdTree Double PolarSeparableFeaturePoint]
libSVMTrainSink labelPath parallelParams trainParams radius = do
  label <- liftIO $ readLabelFile labelPath
  trees <- consume
  let kernel =
        parMapChunk parallelParams rpar (\(x, y) -> similarity x y radius) $!!
        [ (x, y)
        | x <- trees
        , y <- trees ]
  kernelPtr <-
    liftIO $
    MP.sequence $
    P.zipWith getPreComputedKernelFeatureVecPtr [1 ..] $ sp (P.length trees) kernel
  liftIO $ oneVsRestTrain trainParams label kernelPtr
  return trees
  where
    sp _len [] = []
    sp len xs = as : sp len bs
      where
        (as, bs) = P.splitAt len xs

testSink :: Sink (KdTree Double PolarSeparableFeaturePoint) IO [KdTree Double PolarSeparableFeaturePoint]
testSink = do
  xs <- CL.take 2
  if P.length xs > 0
    then do
      let point1 = P.head . KDT.toList . P.head $ xs
          tree1 = P.head xs
          tree2 = P.last xs
      liftIO $ print . size $ tree2
      liftIO $ print . similarity tree1 tree2 $ 10
      return []
    else return []
  ys <- consume
  let trees = xs P.++ ys
      kernel =
        parMap rdeepseq (\(x, y) -> similarity x y 10) $!!
        [ (x, y)
        | x <- trees
        , y <- trees ]
  kernelPtr <-
    liftIO $
    MP.sequence $
    P.zipWith getPreComputedKernelFeatureVecPtr [1 ..] $ sp (P.length trees) kernel
  liftIO $
    P.mapM_
      (\xs -> do
         P.mapM_ (printf "%0.2f ") xs
         putStrLn "") $
    sp (P.length trees) kernel
  return []
  where
    sp _len [] = []
    sp len xs = as : sp len bs
      where
        (as, bs) = P.splitAt len xs


libSVMPredictConduit
  :: ParallelParams
  -> [KdTree Double PolarSeparableFeaturePoint]
  -> Double
  -> Conduit (KdTree Double PolarSeparableFeaturePoint) IO (Ptr C'svm_node)
libSVMPredictConduit parallelParams trees radius = do
  xs <- CL.take (batchSize parallelParams)
  if P.length xs > 0
    then do
      let feature =
            parMapChunk
              parallelParams
              rdeepseq
              (\x -> P.map (\y -> similarity x y radius) trees)
              xs
      ptrs <- liftIO $ MP.mapM (getPreComputedKernelFeatureVecPtr (-1)) feature
      sourceList ptrs
      libSVMPredictConduit parallelParams trees radius
    else return ()
