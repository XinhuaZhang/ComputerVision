module Application.Animal.Conduit where

import           Control.Monad                as M
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           CV.Filter.PinwheelWavelet
import           CV.IO.ImageIO
import           CV.Utility.FFT
import           CV.Utility.Parallel
import           CV.Utility.RepaArrayUtility  (arrayToUnboxed, downsample)
import           Data.Array.Repa              as R
import           Data.Complex
import           Data.Conduit
import           Data.Conduit.List            as CL
import           Data.List                    as L
import           Data.Vector.Unboxed          as VU
import           Data.Vector.Storable          as VS

pinwheelWaveletConduit
  :: ParallelParams
  -> FFTW
  -> PinwheelWaveletParams
  -> Int
  -> Conduit (Double, ImageRepa) (ResourceT IO) (Double, [VU.Vector Double])
pinwheelWaveletConduit parallelParams fftw params@(PinwheelWaveletParams _ _ gs s rs rf af radius) stride = do
  xs <- CL.take (batchSize parallelParams)
  unless
    (L.null xs)
    (do ys <-
          liftIO $
          M.mapM
            (\(label, Image _ x) -> do
               let (Z :. _ :. rows :. cols) = extent x
                   imgVecs =
                     L.map (VU.convert . VU.map (:+ 0)) . arrayToUnboxed $ x
               filters <-
                 makeFilterConvolution
                   fftw
                   (PinwheelWaveletParams rows cols gs s rs rf af radius)
                   Normal :: IO PinwheelWaveletConvolution
               filteredImages <- liftIO $ applyFilterConvolution fftw filters imgVecs
               return (label, filteredImages, (rows, cols)))
            xs
        let zs =
              parMapChunk
                parallelParams
                rdeepseq
                (\(label, filteredImages, (rows, cols)) ->
                    ( label
                    , L.map VU.fromList .
                      L.transpose .
                      (if stride == 1
                         then L.map (VS.toList . VS.map magnitude)
                         else L.map
                                (R.toList .
                                 R.map magnitude .
                                 downsample [stride, stride] .
                                 fromUnboxed (Z :. rows :. cols) . VS.convert)) $
                      filteredImages))
                ys
        sourceList zs
        pinwheelWaveletConduit parallelParams fftw params stride)
