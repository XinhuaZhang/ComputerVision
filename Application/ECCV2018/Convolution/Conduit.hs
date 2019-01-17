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

{-# INLINE polarSeparableFilterConvolutionMagnitude #-}

polarSeparableFilterConvolutionMagnitude
  :: DFTPlan
  -> PolarSeparableFilter PolarSeparableFilterConvolution
  -> [VS.Vector Double]
  -> IO [[VS.Vector Double]]
polarSeparableFilterConvolutionMagnitude plan filter imgVecs = do
  xs <-
    applyPolarSeparableInvariantFilterConvolution plan filter .
    L.map (VS.map (:+ 0)) $
    imgVecs
  return $ L.map (\filteredImage -> L.map (VS.map magnitude) $ filteredImage) xs

{-# INLINE polarSeparableFilterConvolutionRecursive #-}

polarSeparableFilterConvolutionRecursive
  :: DFTPlan
  -> [PolarSeparableFilter PolarSeparableFilterConvolution]
  -> [[[VS.Vector Double]]]
  -> Int
  -> IO [[[[VS.Vector Double]]]]
polarSeparableFilterConvolutionRecursive _ _ _ 0 = error "polarSeparableFilterConvolutionRecursive: layer number is 0."
polarSeparableFilterConvolutionRecursive _ _ xs 1 = return []
polarSeparableFilterConvolutionRecursive plan filters xs n = do
  ys <-
    M.zipWithM
      (\filter (x:_) -> polarSeparableFilterConvolutionMagnitude plan filter x)
      filters
      xs
  zs <- polarSeparableFilterConvolutionRecursive plan filters ys (n - 1)
  return (ys : zs)

polarSeparableFilterConvolutionConduit
  :: ParallelParams
  -> DFTPlan
  -> [PolarSeparableFilter PolarSeparableFilterConvolution]
  -> [PolarSeparableFilter PolarSeparableFilterConvolution]
  -> Int
  -> Conduit (LabeledArray DIM3 Double) (ResourceT IO) (Double, [[[R.Array U DIM3 Double]]])
polarSeparableFilterConvolutionConduit parallelParams plan filters invariantScatteringFilters numLayer = do
  xs <- CL.take (batchSize parallelParams)
  unless
    (L.null xs)
    (do let (Z :. nf :. rows :. cols) =
              (\(LabeledArray _ x) -> extent x) . L.head $ xs
        ys <-
          liftIO $
          MP.mapM
            (\(LabeledArray label x) -> do
               let imgVecs = L.map VU.convert . arrayToUnboxed $ x
               firstLayer <-
                 M.mapM
                   (\filter ->
                      polarSeparableFilterConvolutionMagnitude
                        plan
                        filter
                        imgVecs)
                   filters
               restLayers <-
                 polarSeparableFilterConvolutionRecursive
                   plan
                   invariantScatteringFilters
                   firstLayer
                   numLayer
               return
                 ( fromIntegral label
                 , L.map
                     (L.map
                        (L.map
                           (\zs ->
                              fromUnboxed (Z :. L.length zs :. rows :. cols) .
                              VS.convert . VS.concat $
                              zs))) $
                   (firstLayer : restLayers))) $
          xs
        sourceList ys
        polarSeparableFilterConvolutionConduit
          parallelParams
          plan
          filters
          invariantScatteringFilters
          numLayer)
        

{-# INLINE polarSeparableFilterConvolutionMagnitudeVariedSize #-}

polarSeparableFilterConvolutionMagnitudeVariedSize
  :: DFTPlan
  -> PolarSeparableFilter PolarSeparableFilterConvolution
  -> Int
  -> Int
  -> [VS.Vector Double]
  -> IO [[VS.Vector Double]]
polarSeparableFilterConvolutionMagnitudeVariedSize plan filter rows cols imgVecs = do
  xs <-
    applyPolarSeparableInvariantFilterConvolutionVariedSize
      plan
      filter
      rows
      cols .
    L.map (VS.map (:+ 0)) $
    imgVecs
  return $ L.map (\filteredImage -> L.map (VS.map magnitude) $ filteredImage) xs
  

{-# INLINE polarSeparableFilterConvolutionRecursiveVariedSize #-}

polarSeparableFilterConvolutionRecursiveVariedSize
  :: DFTPlan
  -> [PolarSeparableFilter PolarSeparableFilterConvolution]
  -> Int
  -> Int
  -> [[[VS.Vector Double]]]
  -> Int
  -> IO [[[[VS.Vector Double]]]]
polarSeparableFilterConvolutionRecursiveVariedSize _ _ _ _ _ 0 = error "polarSeparableFilterConvolutionRecursiveVariedSize: layer number is 0."
polarSeparableFilterConvolutionRecursiveVariedSize _ _ _ _ xs 1 = return []
polarSeparableFilterConvolutionRecursiveVariedSize plan filters rows cols xs n = do
  ys <-
    M.zipWithM
      (\filter (x:_) ->
         polarSeparableFilterConvolutionMagnitudeVariedSize
           plan
           filter
           rows
           cols
           x)
      filters
      xs
  zs <-
    polarSeparableFilterConvolutionRecursiveVariedSize
      plan
      filters
      rows
      cols
      ys
      (n - 1)
  return (ys : zs)

polarSeparableFilterConvolutionConduitVariedSize
  :: ParallelParams
  -> DFTPlan
  -> [PolarSeparableFilter PolarSeparableFilterConvolution]
  -> [PolarSeparableFilter PolarSeparableFilterConvolution]
  -> Int
  -> Conduit (LabeledArray DIM3 Double) (ResourceT IO) (Double, [[[R.Array U DIM3 Double]]])
polarSeparableFilterConvolutionConduitVariedSize parallelParams plan filters invariantScatteringFilters numLayer = do
  xs <- CL.take (batchSize parallelParams)
  unless
    (L.null xs)
    (do newPlan <-
          liftIO .
          makePolarSeparableFilterConvolutionPlan
            plan
            (getPolarSeparableFilterParams . L.head $ filters) .
          L.map
            (\(LabeledArray _ arr) ->
               let (Z :. _ :. rows :. cols) = extent arr
               in (rows, cols)) $
          xs
        ys <-
          liftIO $
          MP.mapM
            (\(LabeledArray label x) -> do
               let imgVecs = L.map VU.convert . arrayToUnboxed $ x
                   (Z :. nf :. rows :. cols) = extent x
               if (rows, cols) /=
                  (getPolarSeparableConvolutionFilterDims . L.head $ filters)
                 then do
                   firstLayer <-
                     M.mapM
                       (\filter ->
                          polarSeparableFilterConvolutionMagnitudeVariedSize
                            newPlan
                            filter
                            rows
                            cols
                            imgVecs)
                       filters
                   restLayers <-
                     polarSeparableFilterConvolutionRecursiveVariedSize
                       newPlan
                       invariantScatteringFilters
                       rows
                       cols
                       firstLayer
                       numLayer
                   return
                     ( fromIntegral label
                     , L.map
                         (L.map
                            (L.map
                               (\zs ->
                                  fromUnboxed (Z :. L.length zs :. rows :. cols) .
                                  VS.convert . VS.concat $
                                  zs))) -- . (\x -> [x]) . L.last
                       $
                       (firstLayer : restLayers))
                 else do
                   firstLayer <-
                     M.mapM
                       (\filter ->
                          polarSeparableFilterConvolutionMagnitude
                            newPlan
                            filter
                            imgVecs)
                       filters
                   restLayers <-
                     polarSeparableFilterConvolutionRecursive
                       newPlan
                       invariantScatteringFilters
                       firstLayer
                       numLayer
                   return
                     ( fromIntegral label
                     , L.map
                         (L.map
                            (L.map
                               (\zs ->
                                  fromUnboxed (Z :. L.length zs :. rows :. cols) .
                                  VS.convert . VS.concat $
                                  zs))) -- . (\x -> [x]) . L.last
                       $
                       (firstLayer : restLayers))
               -- filteredImagesList <-
               --   if (rows, cols) /=
               --      (getPolarSeparableConvolutionFilterDims . L.head $ filters)
               --     then M.mapM
               --            (\filter ->
               --               applyPolarSeparableInvariantFilterConvolutionVariedSize
               --                 newPlan
               --                 filter
               --                 rows
               --                 cols
               --                 imgVecs)
               --            filters
               --     else M.mapM
               --            (\filter ->
               --               applyPolarSeparableInvariantFilterConvolution
               --                 newPlan
               --                 filter
               --                 imgVecs)
               --            filters
               -- return
               --   ( fromIntegral label
               --   , [ L.map
               --         (L.map
               --            (\filteredImage ->
               --               fromUnboxed
               --                 (Z :. L.length filteredImage :. rows :. cols) .
               --               -- rescaleUnboxedVector (0, 1) .
               --               VU.map magnitude . VS.convert . VS.concat $
               --               filteredImage))
               --         filteredImagesList
               --     ])
             ) $
          xs
        sourceList ys
        polarSeparableFilterConvolutionConduitVariedSize
          parallelParams
          newPlan
          filters
          invariantScatteringFilters
          numLayer)

-- concatInvariantConduit :: Conduit [[a]] (ResourceT IO)  [a]
-- concatInvariantConduit = awaitForever 

{-# INLINE splitSpace #-}

splitSpace :: String -> [String]
splitSpace xs =
  case L.findIndex (== ' ') xs of
    Nothing -> [xs]
    Just idx ->
      let (as, (_:bs)) = L.splitAt idx xs
      in as : splitSpace bs
