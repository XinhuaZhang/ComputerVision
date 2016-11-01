{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE TypeOperators #-}
module Application.GMM.FisherKernel where

import           Application.GMM.Gaussian
import           Application.GMM.GMM
import           Application.GMM.MixtureModel
import           Control.DeepSeq              as DS
import           Control.Monad.IO.Class
import           CV.CUDA.ArrayUtil
import           CV.CUDA.Context
import           CV.Utility.Parallel
import           Data.Array.Accelerate        as A
import           Data.Array.Accelerate.CUDA   as A
import           Data.Binary
import           Data.ByteString.Lazy         as BL
import           Data.Conduit
import           Data.Conduit.List            as CL
import           Data.List                    as L
import           Data.Maybe
import           Data.Vector                  as V
import           Data.Vector.Unboxed          as VU
import           GHC.Float
import           Prelude                      as P
import           System.IO

fisherVectorW
  :: GMM -> V.Vector Double -> V.Vector GMMData -> VU.Vector Double
fisherVectorW gmm@(MixtureModel n modelVec) zs xs = VU.convert newW
  where !numData = P.fromIntegral . V.length $ xs
        !w1 = (\(Model y) -> P.fst y) . V.head $ modelVec
        !gm1 = V.head $ modelVec
        !z1 = V.head zs
        !newW =
          V.map (\gmk@(Model (wk,_)) ->
                   ((numData * (1 / wk + 1 / w1)) ** (-0.5)) *
                   (V.sum .
                    V.zipWith (\z x ->
                                 (assignPoint gmk z x) / wk -
                                 (assignPoint gm1 z1 x) / w1)
                              zs $
                    xs))
                (V.tail modelVec) -- for i >= 2


fisherVectorMu :: GMM
               -> V.Vector Double
               -> V.Vector GMMData
               -> VU.Vector Double
fisherVectorMu gmm@(MixtureModel n modelVec) zs xs =
  VU.concat . V.toList $ newMuK
  where !numData = P.fromIntegral . V.length $ xs
        !newMuK =
          V.map (\gmk@(Model (wk,(Gaussian _nd muK sigmaK))) ->
                   VU.map (* ((numData * wk) ** (-0.5))) .
                   V.foldl1' (VU.zipWith (+)) .
                   V.zipWith (\z x ->
                                let !assignment = assignPoint gmk z x
                                in VU.zipWith3
                                     (\xd muKd sigmaKd ->
                                        assignment * ((xd - muKd) / sigmaKd))
                                     x
                                     muK
                                     sigmaK)
                             zs $
                   xs)
                modelVec

fisherVectorSigma :: GMM
                  -> V.Vector Double
                  -> V.Vector GMMData
                  -> VU.Vector Double
fisherVectorSigma gmm@(MixtureModel n modelVec) zs xs =
  VU.concat . V.toList $ newSigmaK
  where !numData = P.fromIntegral . V.length $ xs
        !newSigmaK =
          V.map (\gmk@(Model (wk,(Gaussian _nd muK sigmaK))) ->
                   VU.map (* ((2 * numData * wk) ** (-0.5))) .
                   V.foldl1' (VU.zipWith (+)) .
                   V.zipWith (\z x ->
                                let !assignment = assignPoint gmk z x
                                in VU.zipWith3
                                     (\xd muKd sigmaKd ->
                                        assignment *
                                        (((xd - muKd) / sigmaKd) ^ 2 - 1))
                                     x
                                     muK
                                     sigmaK)
                             zs $
                   xs)
                modelVec

fisherVectorConduit
  :: ParallelParams -> GMM -> Conduit (V.Vector GMMData) IO (VU.Vector Double)
fisherVectorConduit parallelParams gmm =
  do xs <- CL.take (batchSize parallelParams)
     if P.length xs > 0
        then let !ys =
                   parMapChunk
                     parallelParams
                     rdeepseq
                     (\x ->
                        let !z =
                              V.map (\y ->
                                       V.foldl' (\s (Model (wj,mj)) ->
                                                   s + (wj * gaussian mj y))
                                                0 $
                                       (model gmm))
                                    x
                            vec =
                              VU.concat . parMap rdeepseq (\f -> f gmm z x) $
                              [fisherVectorMu,fisherVectorSigma]
                            powerNormVec =
                              VU.map (\x ->
                                        if x > 0
                                           then x ** (0.5)
                                           else -((-x) ** (0.5)))
                                     vec
                            !l2Norm =
                              sqrt (VU.foldl' (\a b -> a + b ^ 2) 0 vec)
                        in VU.map (/ l2Norm) vec)
                     xs
             in do sourceList ys
                   fisherVectorConduit parallelParams gmm
        else return ()

fisherVectorSink
  :: ParallelParams -> GMM -> FilePath -> Sink (V.Vector GMMData) IO ()
fisherVectorSink parallelParams gmm filePath =
  do h <- liftIO $ openBinaryFile filePath WriteMode
     go h
  where go handle =
          do xs <- CL.take (batchSize parallelParams)
             if P.length xs > 0
                then let !ys =
                           parMapChunk
                             parallelParams
                             rdeepseq
                             (\x ->
                                let !z =
                                      V.map (\y ->
                                               V.foldl' (\s (Model (wj,mj)) ->
                                                           s +
                                                           (wj * gaussian mj y))
                                                        0 $
                                               (model gmm))
                                            x
                                in VU.toList .
                                   VU.concat . P.map (\f -> f gmm z x) $
                                   [fisherVectorW
                                   ,fisherVectorMu
                                   ,fisherVectorSigma])
                             xs
                         zs = P.map encode ys
                     in do liftIO $ P.mapM_ (BL.hPutStr handle) zs
                           go handle
                else liftIO $ hClose handle


fisherVectorTestSink
  :: ParallelParams -> GMM -> Sink (V.Vector GMMData) IO ()
fisherVectorTestSink parallelParams gmm =
  do x <- await
     case x of
       Nothing -> liftIO $ P.putStrLn "No data"
       Just y ->
         let !zs =
               V.map (\y' ->
                        V.foldl' (\s (Model (wj,mj)) ->
                                    s + (wj * gaussian mj y'))
                                 0 $
                        (model gmm))
                     y
             result =
               VU.toList . VU.concat . P.map (\f -> f gmm zs y) $
               [fisherVectorW,fisherVectorMu,fisherVectorSigma]
             gm0 = (model gmm) V.! 0
             gm1 = (model gmm) V.! 1
             w1 = (\(Model (w1',_)) -> w1') gm1
             w0 = (\(Model (w0',_)) -> w0') gm0
             !numData = P.fromIntegral . V.length $ y
             hehe =
               V.zipWith (\z x' ->
                            (assignPoint gm1 z x') / (w1) -
                            (assignPoint gm0
                                         (V.head zs)
                                         x') /
                            (w0))
                         zs
                         y
             haha = (numData * (1 / w1 + 1 / w0)) ** (-0.5)
         in do liftIO $ print . P.take 5 $ result
               liftIO $ print $ V.take 5 $ hehe
               liftIO $ print haha
               liftIO $ print $ V.sum hehe
               liftIO $
                 print $
                  haha * (V.sum hehe)


fisherVectorAcc :: (Elt a
                   ,IsFloating a)
                => Acc (A.Array DIM1 a)
                -> Acc (A.Array DIM2 a)
                -> Acc (A.Array DIM2 a)
                -> Acc (A.Array DIM2 a)
                -> Acc (A.Array DIM2 (a,a))
fisherVectorAcc w mu sigma x = A.zip fisherVecterMuNK fisherVectorSigmaNK
  where (Z :. n :. d) = unlift $ shape x :: Z :. Exp Int :. Exp Int
        (Z :. k) = unlift $ shape w :: Z :. Exp Int
        xNKD =
          A.replicate (lift (Z :. All :. k :. All))
                      x
        wNK =
          A.replicate (lift (Z :. n :. All))
                      w
        muNKD =
          A.replicate (lift (Z :. n :. All :. All))
                      mu
        sigmaNKD =
          A.replicate (lift (Z :. n :. All :. All))
                      sigma
        normalizedXNKD =
          A.zipWith3 (\a b c -> (a - b) / c)
                     xNKD
                     muNKD
                     sigmaNKD
        normalizedXNKD2 = A.map (^ 2) normalizedXNKD
        gaussianENK =
          A.map (\y -> exp (-0.5 * y)) . A.fold1 (+) $ normalizedXNKD2
        gaussianZNK = A.replicate (lift (Z :. n :. All)) . A.fold1 (*) $ sigma
        wpNK =
          A.zipWith3 (\a b c -> a * b / c)
                     wNK
                     gaussianENK
                     gaussianZNK
        assignmentZNK = A.replicate (lift (Z :. All :. k)) . A.fold1 (+) $ wpNK
        assignmentNK = A.zipWith (/) wpNK assignmentZNK
        assignmentNKD =
          A.replicate (lift (Z :. All :. All :. d))
                      assignmentNK
        fisherVecterMuNK =
          A.zipWith (\w y -> y / (sqrt (A.fromIntegral n * w))) wNK .
          A.fold1 (+) . rotate3D $
          A.zipWith (*) assignmentNKD normalizedXNKD
        fisherVectorSigmaNK =
          A.zipWith (\w y -> y / (sqrt (A.fromIntegral n * w * 2))) wNK .
          A.fold1 (+) . rotate3D $
          A.zipWith (\a b -> a * (b - 1)) assignmentNKD normalizedXNKD2


fisherVectorConduitFloatAcc
  :: ParallelParams -> [Context] -> GMM -> Conduit (V.Vector GMMData) IO (VU.Vector Double)
fisherVectorConduitFloatAcc parallelParams ctx gmm@(MixtureModel k modelVec) =
  do xs <- CL.take (batchSize parallelParams)
     if P.length xs > 0
        then let d = (\(Model (w,(Gaussian d' _ _))) -> d') $ V.head modelVec
                 wAcc =
                   A.use .
                   A.fromList (Z :. k) .
                   P.map double2Float . V.toList . V.map (\(Model (w,gm)) -> w) $
                   modelVec
                 getAcc f =
                   A.use .
                   A.fromList (Z :. k :. d) .
                   P.map double2Float .
                   L.concat .
                   L.transpose .
                   V.toList . V.map (\(Model (w,gm)) -> VU.toList $ f gm) $
                   modelVec
                 muAcc = getAcc mu
                 sigmaAcc = getAcc sigma
                 !xArr =
                   P.map (A.fromList (Z :. ((V.length . P.head $ xs)) :. d) .
                          P.map double2Float . VU.toList . VU.concat . V.toList)
                         xs :: [A.Array DIM2 Float]
                 !yArr =
                   multiGPUStream ctx
                                  (fisherVectorAcc wAcc muAcc sigmaAcc)
                                  xArr
             in do sourceList .
                     P.map (VU.fromList .
                            P.map float2Double .
                            (\(a,b) -> a P.++ b) . P.unzip . A.toList) $
                     yArr
                   fisherVectorConduitFloatAcc parallelParams ctx gmm
        else return ()
