{-# LANGUAGE BangPatterns #-}
module Application.GMM.FisherKernelMPPCA where

import           Application.GMM.Gaussian
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

fisherVectorZ :: MPPCA
              -> V.Vector MPPCAData
              -> VU.Vector Double
fisherVectorZ model'@(MixtureModel _n modelVec) xs = VU.concat . V.toList $ fv
  where
    !invMs = computeInvMS model'
    !ys =
      V.zipWith
        (\im (Model (wj, mj)) -> VU.map (* wj) . V.convert $ ppcaPVec' mj im xs)
        invMs
        modelVec
    !zs = VU.convert $ V.foldl1' (VU.zipWith (+)) ys :: V.Vector Double
    !fv =
      V.zipWith
        (\mixModel@(Model (a, ppcaModel@(PPCA nD _nM w mu' sigma'))) invM ->
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
                  V.zipWith (\x as -> VU.map (* as) $ VU.zipWith (-) x mu') xs assignment
            in VU.map
                 (* (((2 * pi * sigma') ** (P.fromIntegral nD / 2)) /
                     P.fromIntegral (V.length xs) /
                     a)) .
               VU.fromList . Mat.toList $
               matL * wt * invC * (colVector . VU.convert $ xmus))
        modelVec
        invMs


fisherVectorConduit
  :: ParallelParams -> MPPCA -> Conduit (Int,V.Vector MPPCAData) IO (Int,VU.Vector Double)
fisherVectorConduit parallelParams mppca = do
  xs <- CL.take (batchSize parallelParams)
  unless
    (L.null xs)
    (let (!as, !bs) = P.unzip xs
         !ys =
           parMapChunk
             parallelParams
             rdeepseq
             (\x ->
                 let !vec = fisherVectorZ mppca x
                     !l2Norm =
                       sqrt (VU.foldl' (\a b -> a + b ^ (2 :: Int)) 0 vec)
                 in VU.map (/ l2Norm) vec)
             bs
     in do sourceList $ L.zip as ys
           fisherVectorConduit parallelParams mppca)
