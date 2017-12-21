module Application.ECCV2018.Convolution.Conduit where

import           Control.Monad                as M
import           Control.Monad.IO.Class
import           Control.Monad.Parallel       as MP
import           Control.Monad.Trans.Resource
import           CV.Array.LabeledArray
import           CV.Filter.PinwheelWavelet
import           CV.Utility.Parallel
import           CV.Utility.RepaArrayUtility
import           CV.Utility.Utility
import           Data.Array.Repa              as R
import           Data.Complex
import           Data.Conduit
import           Data.Conduit.List            as CL
import           Data.List                    as L
import           Data.Vector.Storable         as VS
import           Data.Vector.Unboxed          as VU


-- output list: layers
filterConvolutionConduit
  :: (FilterConvolution a)
  => ParallelParams
  -> DFTPlan
  -> [a]
  -> Conduit (LabeledArray DIM3 Double) (ResourceT IO) [LabeledArray DIM3 Double]
filterConvolutionConduit parallelParams dftPlan filters = do
  xs <- CL.take (batchSize parallelParams)
  unless
    (L.null xs)
    (do let (Z :. nf :. rows :. cols) =
              (\(LabeledArray _ x) -> extent x) . L.head $ xs
        ys <-
          liftIO $
          MP.mapM
            (\(LabeledArray label x) -> do
               let imgVecs =
                     L.map (VU.convert . VU.map (:+ 0)) . arrayToUnboxed $ x
               filteredImages <-
                 L.concat <$>
                 M.mapM
                   (\filter' -> applyFilterConvolution dftPlan filter' imgVecs)
                   filters
               return
                 [ LabeledArray label .
                   fromUnboxed (Z :. L.length filteredImages :. rows :. cols) .
                 -- rescaleUnboxedVector (0, 1) .
                   VU.map magnitude . VS.convert . VS.concat $
                   filteredImages
                 ])
            xs
        sourceList ys
        filterConvolutionConduit parallelParams dftPlan filters)

-- output list: radius, layers
pinwheelWaveletConvolutionConduit
  :: ParallelParams
  -> DFTPlan
  -> PinwheelWaveletConvolution
  -> Conduit (LabeledArray DIM3 Double) (ResourceT IO) [[LabeledArray DIM3 Double]]
pinwheelWaveletConvolutionConduit parallelParams plan filter = do
  xs <- CL.take (batchSize parallelParams)
  unless
    (L.null xs)
    (do let (Z :. nf :. rows :. cols) =
              (\(LabeledArray _ x) -> extent x) . L.head $ xs
        ys <-
          liftIO $
          MP.mapM
            (\(LabeledArray label x) -> do
               let imgVecs =
                     L.map (VU.convert . VU.map (:+ 0)) . arrayToUnboxed $ x
               filteredImages <-
                 applyPinwheelWaveletConvolution plan filter imgVecs
               return .
                 L.map
                   (\filteredImage ->
                      [ LabeledArray label .
                        fromUnboxed
                          (Z :. L.length filteredImage :. rows :. cols) .
                      -- rescaleUnboxedVector (0, 1) .
                        VU.map magnitude . VS.convert . VS.concat $
                        filteredImage
                      ]) $
                 filteredImages) $
          xs
        sourceList ys
        pinwheelWaveletConvolutionConduit parallelParams plan filter)

listConduit :: Conduit [a] (ResourceT IO) a
listConduit = awaitForever sourceList
