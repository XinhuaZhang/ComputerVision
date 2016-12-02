{-# LANGUAGE BangPatterns #-}
module Application.GMM.FisherKernelMPPCA where

import           Application.GMM.Gaussian
import           Application.GMM.Matrix
import           Application.GMM.MixtureModel
import           Application.GMM.MPPCA
import           Application.GMM.PPCA
import           Control.DeepSeq              as DS
import           Control.Monad
import           Control.Monad.IO.Class
import           CV.Utility.Parallel
import           Data.Binary
import           Data.ByteString.Lazy         as BL
import           Data.Conduit
import           Data.Conduit.List            as CL
import           Data.List                    as L
import           Data.Matrix                  as Mat
import           Data.Maybe
import           Data.Vector                  as V
import           Data.Vector.Unboxed          as VU
import           GHC.Float
import           Prelude                      as P
import           System.IO

type MatrixV = V.Vector (VU.Vector Double)

fisherVectorZ :: MPPCA
              -> V.Vector (Matrix Double)
              -> V.Vector MatrixV
              -> V.Vector MatrixV
              -> V.Vector Double
              -> V.Vector MPPCAData
              -> VU.Vector Double
fisherVectorZ (MixtureModel _n modelVec) invMs invCs wts dets xs =
  VU.concat . V.toList $ fv
  where
        -- !invMs = computeInvMS model'
        !ys =
          V.zipWith3 (\ic (Model (wj,mj)) det ->
                       VU.map (* wj) . V.convert $ ppcaPVec'' mj ic det xs)
                    invCs
                    modelVec
                    dets
        !zs = VU.convert $ V.foldl1' (VU.zipWith (+)) ys :: V.Vector Double
        !fv =
          V.zipWith4
            (\mixModel@(Model (a,ppcaModel@(PPCA nD _nM w mu' sigma'))) invC wt det ->
               let
                   -- !invC = computeInvC ppcaModel invM
                   -- !wt = Mat.transpose w
                   -- !wtw = wt * w
                   -- !invWtw =
                   --   case inverse wtw of
                   --     Left msg  -> error msg
                   --     Right inv -> inv
                   -- !scaledInvWtw = scaleMatrix sigma' invWtw
                   -- !matL = Mat.transpose $ cholDecomp scaledInvWtw
                   !assignment = assignPointVec' mixModel invC det zs xs
                   !xmus =
                     V.foldl1' (VU.zipWith (+)) $
                     V.zipWith (\x as -> VU.map (* as) $ VU.zipWith (-) x mu') xs assignment
               in VU.map (* (((2 * pi * sigma') ** (P.fromIntegral nD / 2)) /
                             P.fromIntegral (V.length xs) /
                             a)) .
                  matrixVecMult wt . matrixVecMult invC $
                  xmus)
            -- VU.fromList . Mat.toList $
            -- wt * invC * (colVector . VU.convert $ xmus))
            modelVec
            invCs
            wts
            dets

fisherVectorZM :: MPPCA
               -> V.Vector MPPCAData
               -> IO (VU.Vector Double)
fisherVectorZM model'@(MixtureModel _n modelVec) xs =
  do !fv <-
       V.zipWithM (\mixModel@(Model (a,ppcaModel@(PPCA nD _nM w mu' sigma'))) invM ->
                    let !invC = computeInvC ppcaModel invM
                        !wt = Mat.transpose w
                        !wtw = wt * w
                        !invWtw =
                          case inverse wtw of
                            Left msg  -> error msg
                            Right inv -> inv
                        !scaledInvWtw = scaleMatrix sigma' invWtw
                        !matL = Mat.transpose $ cholDecomp scaledInvWtw
                        !assignment = assignPointVec mixModel invM zs xs
                        !xmus =
                          V.foldl1' (VU.zipWith (+)) $
                          V.zipWith (\x as ->
                                       VU.map (* as) $ VU.zipWith (-) x mu')
                                    xs
                                    assignment
                        !a' = invC * (colVector . VU.convert $ xmus)
                        !b = wt * a'
                        !c = matL * b
                    in do -- System.IO.putStrLn "scaledInvWtw"
                          -- print scaledInvWtw
                          -- System.IO.putStrLn "matL"
                          -- print matL
                          -- System.IO.putStrLn "invC * (colVector . VU.convert $ xmus)"
                          -- print $ Mat.toList a'
                          -- System.IO.putStrLn "wt * a'"
                          -- print $ Mat.toList b
                          -- System.IO.putStrLn "matL * b"
                          -- print $ Mat.toList c
                          System.IO.putStrLn "weight"
                          print a
                          System.IO.putStrLn "Var"
                          P.mapM_ print $ P.map (P.map round) . Mat.toLists $ elementwiseUnsafe (+) (diagonal 0 (V.replicate nD sigma')) (w * wt)
                          return $
                            VU.map (* (((2 * pi * sigma') **
                                        (P.fromIntegral nD / 2)) /
                                       P.fromIntegral (V.length xs) /
                                       a)) .
                            VU.fromList . Mat.toList $
                            matL * wt * invC * (colVector . VU.convert $ xmus))
                 modelVec
                 invMs
     return $ VU.concat . V.toList $ fv
  where !invMs = computeInvMS model'
        !ys =
          V.zipWith (\im (Model (wj,mj)) ->
                       VU.map (* wj) . V.convert $ ppcaPVec' mj im xs)
                    invMs
                    modelVec
        !zs = VU.convert $ V.foldl1' (VU.zipWith (+)) ys :: V.Vector Double



fisherVectorConduit
  :: ParallelParams -> MPPCA -> Conduit (Int,V.Vector MPPCAData) IO (Int,VU.Vector Double)
fisherVectorConduit parallelParams mppca@(MixtureModel _n modelVec) =
  do xs <- CL.take (batchSize parallelParams)
     unless (L.null xs)
            (let (!as,!bs) = P.unzip xs
                 !ys =
                   parMapChunk
                     parallelParams
                     rdeepseq
                     (\x ->
                        let !vec = fisherVectorZ mppca invMs invCs wts dets x
                            !l2Norm =
                              sqrt (VU.foldl' (\a b -> a + b ^ (2 :: Int)) 0 vec)
                        in VU.map (/ l2Norm) vec)
                     bs
             in do sourceList $ L.zip as ys
                   fisherVectorConduit parallelParams mppca)
  where !invMs = computeInvMS mppca
        !invCs =
          V.zipWith (\(Model (_,ppcaModel)) invM ->
                       matrix2MatrixV $ computeInvC ppcaModel invM)
                    modelVec
                    invMs
        !wts =
          V.map (\(Model (_,ppcaModel)) ->
                   matrix2MatrixV . Mat.transpose $ wMat ppcaModel)
                modelVec
        !dets =
          V.map (\(Model (_,ppcaModel@(PPCA nd _nzd _w' _mu' sigma'))) ->
                   let w = wMat ppcaModel
                       wt = Mat.transpose $ wMat ppcaModel
                       c =
                         elementwiseUnsafe (+)
                                           (diagonal 0 (V.replicate nd sigma'))
                                           (w * wt)
                   in detLU c)
                modelVec

fisherVectorConduitM
  :: ParallelParams
  -> MPPCA
  -> Conduit (Int,V.Vector MPPCAData) IO (Int,VU.Vector Double)
fisherVectorConduitM _parallelParams mppca =
  awaitForever
    (\(label,x) ->
       do y <- liftIO $ fisherVectorZM mppca x
          liftIO $ print y
          yield (label,y))
