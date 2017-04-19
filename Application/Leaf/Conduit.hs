module Application.Leaf.Conduit where

import           Classifier.LibLinear
--import           Classifier.LibSVM
import           Control.Arrow
import           Control.Monad                as M
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           CV.Utility.Parallel
import           Data.Array.Repa              as R
import           Data.Complex
import           Data.Conduit
import           Data.Conduit.List            as CL
import           Data.List                    as L
import           Data.Vector                  as V
import           Data.Vector.Unboxed          as VU
import           Foreign.Marshal.Array
import           Foreign.Ptr


{-# INLINE complexDistance #-}

complexDistance :: VU.Vector (Complex Double)
                -> VU.Vector (Complex Double)
                -> Double
complexDistance vec1 vec2 =
  (VU.sum $ VU.zipWith (\x y -> (-1) *  magnitude (x - y)) vec1 vec2) / (fromIntegral . VU.length $ vec1)


complexKernelP :: [VU.Vector (Complex Double)] -> IO [[Double]]
complexKernelP xs = do
  arr <-
    computeUnboxedP $
    fromFunction (Z :. n :. n) $ \(Z :. j :. i) ->
      if j <= i
        then complexDistance (vec V.! i) (vec V.! j)
        else 0
  return
    [ [ if j <= i
      then arr R.! (Z :. j :. i)
      else arr R.! (Z :. i :. j)
    | i <- [0 .. n - 1]
    ]
    | j <- [0 .. n - 1]
    ]
  where
    n = L.length xs
    vec = V.fromList xs


complexKernel :: ParallelParams -> [VU.Vector (Complex Double)] -> [[Double]]
complexKernel parallelParams xs = do
  parMapChunk
    parallelParams
    rdeepseq
    (\j ->
       [ if j <= i
         then arr R.! (Z :. j :. i)
         else arr R.! (Z :. i :. j)
       | i <- [0 .. n - 1]
       ])
    [0 .. n - 1]
  where
    n = L.length xs
    vec = V.fromList xs
    arr =
      fromFunction (Z :. n :. n) $ \(Z :. j :. i) ->
        if j <= i
          then complexDistance (vec V.! i) (vec V.! j)
          else 0


-- libSVMPredictConduit
--   :: ParallelParams
--   -> [VU.Vector (Complex Double)]
--   -> Conduit (Double, VU.Vector (Complex Double)) (ResourceT IO) (Double, Ptr C'svm_node)
-- libSVMPredictConduit parallelParams trainFeatures = do
--   xs <- CL.take (batchSize parallelParams)
--   unless
--     (L.null xs)
--     (do let (labels, ys) = L.unzip xs
--             zs =
--               parMapChunk
--                 parallelParams
--                 rdeepseq
--                 (\y -> L.map (complexDistance y) trainFeatures)
--                 ys
--         ptrs <- liftIO $ M.mapM (getPreComputedKernelFeatureVecPtr (-1)) zs
--         sourceList $ L.zip labels ptrs
--         libSVMPredictConduit parallelParams trainFeatures)


featurePtrConduit :: Conduit (a, VU.Vector Double) (ResourceT IO) (a, Ptr C'feature_node)
featurePtrConduit =
  awaitForever
    (\(label, vec) -> do
       featurePtr <- liftIO $ newArray . getFeature . Dense . VU.toList $ vec
       yield (label, featurePtr))


featureConduitP
  :: ParallelParams
  -> Conduit (a,VU.Vector Double) (ResourceT IO) (a, [C'feature_node])
featureConduitP parallelParams = do
  xs <- CL.take (batchSize parallelParams)
  unless
    (L.null xs)
    (do let ys =
              parMapChunk
                parallelParams
                rseq
                (second $ getFeature . Dense . VU.toList) $
              xs
        CL.sourceList ys
        featureConduitP parallelParams)
