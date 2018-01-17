module Application.ECCV2018.Convolution.Conduit where

import           Control.Monad                  as M
import           Control.Monad.IO.Class
import           Control.Monad.Parallel         as MP
import           Control.Monad.Trans.Resource
import           CV.Array.LabeledArray
-- import           CV.Filter.PinwheelWavelet
import           CV.Filter.PolarSeparableFilter
import           CV.Utility.Parallel
import           CV.Utility.RepaArrayUtility
import           CV.Utility.Utility
import           Data.Array.Repa                as R
import           Data.Complex
import           Data.Conduit
import           Data.Conduit.List              as CL
import           Data.List                      as L
import           Data.Vector.Storable           as VS
import           Data.Vector.Unboxed            as VU


-- output list: layers
-- filterConvolutionConduit
--   :: ParallelParams
--   -> DFTPlan
--   -> PolarSeparableFilterConvolution
--   -> Conduit (LabeledArray DIM3 Double) (ResourceT IO) [[LabeledArray DIM3 Double]]
-- filterConvolutionConduit parallelParams dftPlan filters = do
--   xs <- CL.take (batchSize parallelParams)
--   unless
--     (L.null xs)
--     (do let (Z :. nf :. rows :. cols) =
--               (\(LabeledArray _ x) -> extent x) . L.head $ xs
--         ys <-
--           liftIO $
--           MP.mapM
--             (\(LabeledArray label x) -> do
--                let imgVecs =
--                      L.map (VU.convert . VU.map (:+ 0)) . arrayToUnboxed $ x
--                filteredImages <-
--                  applyPolarSeparableFilterConvolution dftPlan filters imgVecs
--                return
--                  [ [ LabeledArray label .
--                      fromUnboxed (Z :. L.length filteredImages :. rows :. cols) .
--                      -- rescaleUnboxedVector (0, 1) .
--                      VU.map magnitude . VS.convert . VS.concat $
--                      filteredImages
--                    ]
--                  ])
--             xs
--         sourceList ys
--         filterConvolutionConduit parallelParams dftPlan filters)

-- -- output list: layers, free degrees
-- pinwheelWaveletConvolutionConduit
--   :: ParallelParams
--   -> DFTPlan
--   -> PinwheelWaveletFilterConvolution
--   -> Conduit (LabeledArray DIM3 Double) (ResourceT IO) [[LabeledArray DIM3 Double]]
-- pinwheelWaveletConvolutionConduit parallelParams plan filter = do
--   xs <- CL.take (batchSize parallelParams)
--   unless
--     (L.null xs)
--     (do let (Z :. nf :. rows :. cols) =
--               (\(LabeledArray _ x) -> extent x) . L.head $ xs
--         ys <-
--           liftIO $
--           MP.mapM
--             (\(LabeledArray label x) -> do
--                let imgVecs =
--                      L.map (VU.convert . VU.map (:+ 0)) . arrayToUnboxed $ x
--                filteredImages <-
--                  applyPinwheelWaveletFilterConvolution plan filter imgVecs
--                return
--                  [ L.map
--                      (\filteredImage ->
--                         LabeledArray label .
--                         fromUnboxed
--                           (Z :. L.length filteredImage :. rows :. cols) .
--                         -- rescaleUnboxedVector (0, 1) .
--                         VU.map magnitude . VS.convert . VS.concat $
--                         filteredImage) $
--                    filteredImages
--                  ]) $
--           xs
--         sourceList ys
--         pinwheelWaveletConvolutionConduit parallelParams plan filter)
        

polarSeparableFilterConvolutionConduit
  :: ParallelParams
  -> DFTPlan
  -> [PolarSeparableFilter PolarSeparableFilterConvolution]
  -> Conduit (LabeledArray DIM3 Double) (ResourceT IO) (Double, [[[R.Array U DIM3 Double]]])
polarSeparableFilterConvolutionConduit parallelParams plan filters = do
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
               filteredImagesList <-
                 M.mapM
                   (\filter ->
                      applyPolarSeparableInvariantFilterConvolution
                        plan
                        filter
                        imgVecs)
                   filters
               return
                 ( fromIntegral label
                 , [ L.map
                       (L.map
                          (\filteredImage ->
                             fromUnboxed
                               (Z :. L.length filteredImage :. rows :. cols) .
                                                       -- rescaleUnboxedVector (0, 1) .
                             VU.map magnitude . VS.convert . VS.concat $
                             filteredImage))
                       filteredImagesList
                   ])) $
          xs
        sourceList ys
        polarSeparableFilterConvolutionConduit parallelParams plan filters)

-- concatInvariantConduit :: Conduit [[a]] (ResourceT IO)  [a]
-- concatInvariantConduit = awaitForever 
