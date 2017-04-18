{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE FlexibleContexts #-}

module CV.V4Filter
  ( module V4
  , SeparableFilterParams(..)
  , V4SeparableFilterParamsAxis(..)
  , generateV4SeparableFilterAxis
  , generateV4SeparableFilterWithCenterAxis
  , applyV4SeparableFilterLabeledArrayConduit
  , applyV4SeparableFilter
  -- , applyV4SeparableFilterComplexLabeledArrayConduit
  -- , filterNum
  -- , filterNumComplex
  ) where

import           Control.Arrow
import           Control.Monad                    as M
import           Control.Monad.Trans.Resource
import           CV.Array.LabeledArray
import           CV.Filter.CartesianGratingFilter as V4
import           CV.Filter.HyperbolicFilter       as V4
import           CV.Filter.PolarSeparableFilter   as V4 hiding (makeFilter)
import           CV.FilterExpansion               as V4
import           CV.Utility.Coordinates
import           CV.Utility.Parallel
import           CV.Utility.RepaArrayUtility
import           Data.Array.Repa                  as R
import           Data.Complex
import           Data.Conduit
import           Data.Conduit.List                as CL
import           Data.List                        as L
import           Data.Vector.Unboxed              as VU

data SeparableFilterParams
  = P
  | C
  | H
  | PC
  | PH
  | CH
  | PCH
  deriving (Show, Read)

data V4SeparableFilterParamsAxis = V4SeparableFilterParamsAxis
  { separableFilterRows             :: !Int
  , separableFilterCols             :: !Int
  , polarSeparableScale             :: ![Double]
  , polarSeparableFreq              :: ![Int]
  , polarSeparableRadialMultiplier  :: ![Int]
  , polarSeparableAngularMultiplier :: ![Int]
  , cartesianGratingScale           :: ![Double]
  , cartesianGratingFreq            :: ![Double]
  , cartesianGratingAngle           :: ![Double]
  , hyperbolicSeparableScale        :: ![Double]
  , hyperbolicSeparableUFreq        :: ![Int]
  , hyperbolicSeparableVFreq        :: ![Int]
  , hyperbolicSeparableAngle        :: !Double
  , separableFilterParams           :: !SeparableFilterParams
  } deriving (Show, Read)

generateV4SeparableFilterAxis :: V4SeparableFilterParamsAxis -> [V4SeparableFilter]
generateV4SeparableFilterAxis params =
  let rows = div (separableFilterRows params) 2
      cols = div (separableFilterCols params) 2
  in generateV4SeparableFilterWithCenterAxis params (rows, cols)


generateV4SeparableFilterWithCenterAxis :: V4SeparableFilterParamsAxis -> (Int,Int) -> [V4SeparableFilter]
generateV4SeparableFilterWithCenterAxis params (rows, cols) =
  let polarSeparableFilterParams =
        PolarSeparableFilterParamsAxis
        { getPolarSeparableFilterAxisRows = separableFilterRows params
        , getPolarSeparableFilterAxisCols = separableFilterCols params
        , getPolarSeparableFilterAxisScale = polarSeparableScale params
        , getPolarSeparableFilterAxisFreq = polarSeparableFreq params
        , getPolarSeparableFilterAxisRadialMultiplier =
          polarSeparableRadialMultiplier params
        , getPolarSeparableFilterAxisAngularMultiplier =
          polarSeparableAngularMultiplier params
        }
      cartesianGratingFilterParams =
        CartesianGratingFilterParams
        { getCartesianGratingFilterRows = separableFilterRows params
        , getCartesianGratingFilterCols = separableFilterCols params
        , getCartesianGratingFilterScale = cartesianGratingScale params
        , getCartesianGratingFilterFreq = cartesianGratingFreq params
        , getCartesianGratingFilterAngle = cartesianGratingAngle params
        }
      hfAngle = hyperbolicSeparableAngle params
      hyperbolicSeparableFilterParams =
        HyperbolicSeparableFilterParams
        { getHyperbolicSeparableFilterRows = separableFilterRows params
        , getHyperbolicSeparableFilterCols = separableFilterCols params
        , getHyperbolicSeparableFilterScale = hyperbolicSeparableScale params
        , getHyperbolicSeparableFilterUFreq = hyperbolicSeparableUFreq params
        , getHyperbolicSeparableFilterVFreq = hyperbolicSeparableVFreq params
        , getHyperbolicSeparableFilterAngle = [0,hfAngle .. 90 - hfAngle]
        }
      psf =
        getFilterVectors
          (makeFilter
             (PolarSeparableFilter polarSeparableFilterParams Null :: PolarSeparableFilterExpansionAxis)
             (rows, cols))
      cgf =
        getFilterVectors
          (makeFilter
             (CartesianGratingFilter cartesianGratingFilterParams Null :: CartesianGratingFilter)
             (rows, cols))
      hf =
        getFilterVectors
          (makeFilter
             (HyperbolicSeparableFilter hyperbolicSeparableFilterParams Null :: HyperbolicSeparableFilter)
             (rows, cols))
  in case separableFilterParams params of
       P -> [psf]
       C -> [cgf]
       H -> [hf]
       PC -> [psf, cgf]
       PH -> [psf, hf]
       CH -> [cgf, hf]
       PCH -> [psf, cgf, hf]

-- applyV4QuadTreeFilterConduit
--   :: (R.Source s Double)
--   => ParallelParams
--   -> V4QuadTreeFilter
--   -> Conduit (R.Array s DIM3 Double) (ResourceT IO) [[VU.Vector Double]]
-- applyV4QuadTreeFilterConduit parallelParams filters = do
--   xs <- CL.take (batchSize parallelParams)
--   unless
--     (L.null xs)
--     (do let ys =
--               parMapChunk
--                 parallelParams
--                 rdeepseq
--                 (\x ->
--                     let (Z :. channels :. _ :. _) = extent x
--                         imgVecs =
--                           L.map
--                             (\i ->
--                                 VU.map (:+ 0) . toUnboxed . computeS . R.slice x $
--                                 (Z :. i :. All :. All))
--                             [0 .. channels - 1]
--                     in L.map (applyFilter imgVecs) filters)
--                 xs
--         sourceList ys
--         applyV4QuadTreeFilterConduit parallelParams filters)


applyV4SeparableFilterLabeledArrayConduit
  :: ParallelParams
  -> [V4SeparableFilter]
  -> Conduit (LabeledArray DIM3 Double) (ResourceT IO) (Double, VU.Vector Double)
applyV4SeparableFilterLabeledArrayConduit parallelParams filters = do
  xs <- CL.take (batchSize parallelParams)
  unless
    (L.null xs)
    (do let ys =
              parMapChunk
                parallelParams
                rdeepseq
                (\(LabeledArray l x) ->
                    let (Z :. channels :. _ :. _) = extent x
                        imgVecs =
                          L.map
                            (\i ->
                                VU.map (:+ 0) . toUnboxed . computeS . R.slice x $
                                (Z :. i :. All :. All))
                            [0 .. channels - 1]
                    in ( fromIntegral l
                       , normalizeVec .
                         VU.concat .
                         L.map
                           (\filter' ->
                               VU.concat $
                               L.map (applyV4SeparableFilter filter') imgVecs) $
                         filters))
                xs
        sourceList ys
        applyV4SeparableFilterLabeledArrayConduit parallelParams filters)

-- applyFilterVariedSizeConduit
--   :: (R.Source s Double)
--   => ParallelParams
--   -> PolarSeparableFilterParamsGrid
--   -> CartesianGratingFilterParams
--   -> HyperbolicFilterParams
--   -> Conduit (R.Array s DIM3 Double) (ResourceT IO) [VU.Vector Double]
-- applyFilterVariedSizeConduit parallelParams polarFilterParams cartesianGratingFilterParams hyperbolicFilterParams = do
--   xs <- CL.take (batchSize parallelParams)
--   unless
--     (L.null xs)
--     (do let ys =
--               parMapChunk
--                 parallelParams
--                 rdeepseq
--                 (\x ->
--                     let (Z :. channels :. rows :. cols) = extent x
--                         psf =
--                           makeFilter . changeSizeParameter rows cols $
--                           PolarSeparableFilter polarFilterParams [] :: PolarSeparableFilterExpansion
--                         cgf =
--                           makeFilter . changeSizeParameter rows cols $
--                           CartesianGratingFilter cartesianGratingFilterParams [] :: CartesianGratingFilter
--                         hf =
--                           makeFilter . changeSizeParameter rows cols $
--                           HyperbolicFilter hyperbolicFilterParams [] :: HyperbolicFilter
--                         psfVecs = getFilterVectors psf
--                         cgfVecs = getFilterVectors cgf
--                         hfVecs = getFilterVectors hf
--                         filterVecsList =
--                           L.zipWith3
--                             (\a b c -> a L.++ b L.++ c)
--                             psfVecs
--                             cgfVecs
--                             hfVecs
--                         downSampleFactor =
--                           getPolarSeparableFilterDownsampleFactor polarFilterParams
--                         img =
--                           downsample [downSampleFactor, downSampleFactor, 1] x
--                         imgVecs =
--                           L.map
--                             (\i ->
--                                 VU.map (:+ 0) . toUnboxed . computeS . R.slice img $
--                                 (Z :. i :. All :. All))
--                             [0 .. channels - 1]
--                     in applyFilter imgVecs filterVecsList)
--                 xs
--         sourceList ys
--         applyFilterVariedSizeConduit
--           parallelParams
--           polarFilterParams
--           cartesianGratingFilterParams
--           hyperbolicFilterParams)

-- applyFilterFixedSizeConduit
--   :: (R.Source s Double)
--   => ParallelParams
--   -> Int
--   -> [[VU.Vector (Complex Double)]]
--   -> Conduit (R.Array s DIM3 Double) (ResourceT IO) [VU.Vector Double]
-- applyFilterFixedSizeConduit parallelParams downSampleFactor filterVecsList = do
--   xs <- CL.take (batchSize parallelParams)
--   unless
--     (L.null xs)
--     (do let ys =
--               parMapChunk
--                 parallelParams
--                 rdeepseq
--                 (\x ->
--                     let (Z :. channels :. _ :. _) = extent x
--                         img =
--                           downsample [downSampleFactor, downSampleFactor, 1] x
--                         imgVecs =
--                           L.map
--                             (\i ->
--                                 VU.map (:+ 0) . toUnboxed . computeS . R.slice img $
--                                 (Z :. i :. All :. All))
--                             [0 .. channels - 1]
--                     in applyFilter imgVecs filterVecsList)
--                 xs
--         sourceList ys
--         applyFilterFixedSizeConduit parallelParams downSampleFactor filterVecsList)

{-# INLINE normalizeVec #-}

normalizeVec :: VU.Vector Double -> VU.Vector Double
normalizeVec vec
  | s == 0 = VU.replicate (VU.length vec) 0
  | otherwise = VU.map (/ s) vec
  where
    s = sqrt . VU.sum . VU.map (^ 2) $ vec

{-# INLINE complexVec2RealVec #-}

complexVec2RealVec :: VU.Vector (Complex Double) -> VU.Vector Double
complexVec2RealVec vec = a VU.++ b
  where
    (a, b) = VU.unzip . VU.map polar $ vec

-- {-# INLINE applyFilter #-}

-- applyFilter :: [VU.Vector (Complex Double)]
--             -> [[VU.Vector (Complex Double)]]
--             -> [VU.Vector Double]
-- applyFilter imgVecs =
--   L.map
--     (\filterVecs ->
--         normalizeVec .
--         complexVec2RealVec .
--         VU.fromList .
--         L.concatMap
--           (\imgVec -> L.map (VU.sum . VU.zipWith (*) imgVec) filterVecs) $
--         imgVecs)

{-# INLINE normalizeComplex #-}

normalizeComplex :: Complex Double -> (Double, Complex Double)
normalizeComplex x =
  ( mag
  , if mag == 0
      then 0
      else x / (mag :+ 0))
  where
    mag = magnitude x

{-# INLINE computePhaseDifference #-}

computePhaseDifference :: [Double] -> [Complex Double] -> [Double]
computePhaseDifference [] _ = []
computePhaseDifference _ [] = []
computePhaseDifference (0:fs) (_:xs) = computePhaseDifference fs xs
computePhaseDifference (fn:fs) (xn:xs) =
  L.concat
    (L.zipWith
       (\fm xm ->
           let mn = fromIntegral $ lcm (round fm) (round fn) :: Double
               (!y :+ (!z)) =
                 (xn ** (mn / fn :+ 0)) * (conjugate $ xm ** (mn / fm :+ 0))
           in [y, z])
       fs
       xs) L.++
  computePhaseDifference fs xs

{-# INLINE computeMagnitudeContrastInvarient #-}

computeMagnitudeContrastInvarient :: [Double] -> [Double] -> [Double]
computeMagnitudeContrastInvarient [] _ = []
computeMagnitudeContrastInvarient _ [] = []
computeMagnitudeContrastInvarient (0:fs) (_:xs) =
  computeMagnitudeContrastInvarient fs xs
computeMagnitudeContrastInvarient (_:fs) (xn:xs) =
  L.map (\xm -> sqrt $ xn * xm) xs L.++ computeMagnitudeContrastInvarient fs xs

{-# INLINE applyFilter #-}

applyFilter :: VU.Vector (Complex Double)
            -> [VU.Vector (Complex Double)]
            -> [Complex Double]
applyFilter imgVec = L.map (VU.sum . VU.zipWith (*) imgVec)


{-# INLINE applyV4SeparableFilter #-}

applyV4SeparableFilter :: V4SeparableFilter
                       -> VU.Vector (Complex Double)
                       -> VU.Vector Double
applyV4SeparableFilter (V4PolarSeparableFilterAxis freqs filters) imgVec =
  let (mags, normalizedXS) =
        L.unzip . L.map (L.unzip . L.map normalizeComplex . applyFilter imgVec) $
        filters
  in VU.fromList $ L.concat mags --L.++ L.concatMap (computePhaseDifference freqs) normalizedXS
applyV4SeparableFilter (V4PolarSeparableFilterGrid (rfs, afs) filters) imgVec =
  VU.concat .
  L.map
    (uncurry (VU.++) .
     join
       (***)
       (\xs ->
           let (mags, normalizedXS) =
                 L.unzip .
                 L.map (L.unzip . L.map normalizeComplex . applyFilter imgVec) $
                 xs
           in VU.fromList $
              L.concat mags L.++ L.concatMap (computePhaseDifference afs) normalizedXS L.++
              (computePhaseDifference rfs . L.head . L.transpose $ normalizedXS))) $
  filters
applyV4SeparableFilter (V4CartesianSeparableFilter freqs filters) imgVec =
  let (mags, normalizedXS) =
        L.unzip . L.map (L.unzip . L.map normalizeComplex . applyFilter imgVec) $
        filters
  in VU.fromList $
     L.concat mags  -- L.++ L.concatMap (computePhaseDifference freqs) normalizedXS
applyV4SeparableFilter (V4HyperbolicSeparableFilter filters) imgVec =
  let xs = applyFilter imgVec filters
  in VU.fromList . L.map magnitude $xs
applyV4SeparableFilter _ _ = error "applyV4SeparableFilter: filter type is not supported."



-- filterNum :: V4SeparableFilter -> Int
-- filterNum (V4PolarSeparableFilter (rfs, afs) xs) =
--   k * (m * n * 2 +
--   --m * (n - 1) * (n - 2)  + n * (m - 1) * (m - 2)  -- +
--    m * (n - 1) * (n - 2) * 2 + n * (m - 1) * (m - 2) * 2
--   ) -- * 2
--   where
--     n = L.length rfs
--     m = L.length afs
--     k = L.length xs
-- filterNum (V4CartesianSeparableFilter _ xs) =
--   numAxes * ( --div (numFreq * (numFreq - 1)) 2
--   numFreq -- +
--              -- (div  (numFreq * (numFreq - 1))  2) * 2
--             )
--   where
--     numAxes = L.length xs
--     numFreq = L.length . L.head $ xs



{-# INLINE computePhaseDifferencePhase #-}

computePhaseDifferencePhase :: [Double] -> [Complex Double] -> [Double]
computePhaseDifferencePhase [] [] = []
computePhaseDifferencePhase (0:fs) (_:xs) = computePhaseDifferencePhase fs xs
computePhaseDifferencePhase (fn:fs) (xn:xs) =
  (L.zipWith
     (\fm xm ->
        let mn = fromIntegral $ lcm (round fm) (round fn)
        in phase $ (xn ** (mn / fn :+ 0)) * (conjugate $ xm ** (mn / fm :+ 0)))
     fs
     xs) L.++
  computePhaseDifferencePhase fs xs

-- {-# INLINE applyV4SeparableFilterComplex #-}

-- applyV4SeparableFilterComplex :: V4SeparableFilter
--                               -> VU.Vector (Complex Double)
--                               -> VU.Vector (Complex Double)
-- applyV4SeparableFilterComplex (V4PolarSeparableFilter (rfs, afs) filters) imgVec =
--   VU.concat .
--   L.map
--     (uncurry (VU.++) .
--      join
--        (***)
--        (\xs ->
--           let (mags, normalizedXS) =
--                 L.unzip .
--                 L.map (L.unzip . L.map normalizeComplex . applyFilter imgVec) $
--                 xs
--           in VU.fromList $
--              (L.concat $
--               L.zipWith
--                 (L.zipWith mkPolar)
--                 (L.map (computeMagnitudeContrastInvarient afs) mags)
--                 (L.map (computePhaseDifferencePhase afs) normalizedXS)) L.++
--              (L.concat $
--               L.zipWith
--                 (L.zipWith mkPolar)
--                 (L.map (computeMagnitudeContrastInvarient rfs) . L.transpose $
--                  mags)
--                 (L.map (computePhaseDifferencePhase rfs) . L.transpose $
--                  normalizedXS)))) $
--   filters
-- applyV4SeparableFilterComplex (V4CartesianSeparableFilter freqs filters) imgVec =
--   let (mags, normalizedXS) =
--         L.unzip . L.map (L.unzip . L.map normalizeComplex . applyFilter imgVec) $
--         filters
--   in VU.fromList $
--      L.concat $
--      L.zipWith
--        (L.zipWith mkPolar)
--        (L.map (computeMagnitudeContrastInvarient freqs) mags)
--        (L.map (computePhaseDifferencePhase freqs) normalizedXS)
-- applyV4SeparableFilterComplex _ _ = error "applyV4SeparableFilter: filter type is not supported."



-- filterNumComplex :: V4SeparableFilter -> Int
-- filterNumComplex (V4PolarSeparableFilter (rfs, afs) xs) =
--   k * (m * (n - 1) * (n - 2) + n * (m - 1) * (m - 2))
--   where
--     n = L.length rfs
--     m = L.length afs
--     k = L.length xs
-- filterNumComplex (V4CartesianSeparableFilter _ xs) =
--   numAxes * (div (numFreq * (numFreq - 1)) 2)
--   where
--     numAxes = L.length xs
--     numFreq = L.length . L.head $ xs



-- applyV4SeparableFilterComplexLabeledArrayConduit
--   :: ParallelParams
--   -> [V4SeparableFilter]
--   -> Conduit (LabeledArray DIM3 Double) (ResourceT IO) (Double, VU.Vector (Complex Double))
-- applyV4SeparableFilterComplexLabeledArrayConduit parallelParams filters = do
--   xs <- CL.take (batchSize parallelParams)
--   unless
--     (L.null xs)
--     (do let ys =
--               parMapChunk
--                 parallelParams
--                 rdeepseq
--                 (\(LabeledArray l x) ->
--                    let (Z :. channels :. _ :. _) = extent x
--                        imgVecs =
--                          L.map
--                            (\i ->
--                               VU.map (:+ 0) . toUnboxed . computeS . R.slice x $
--                               (Z :. i :. All :. All))
--                            [0 .. channels - 1]
--                        complexVec =
--                          VU.concat .
--                          L.map
--                            (\filter' ->
--                               VU.concat $
--                               L.map
--                                 (applyV4SeparableFilterComplex filter')
--                                 imgVecs) $
--                          filters
--                        normalizedMagVec =
--                          normalizeVec . VU.map magnitude $ complexVec
--                    in ( fromIntegral l
--                       , VU.zipWith
--                           (\a b -> mkPolar a . phase $ b)
--                           normalizedMagVec
--                           complexVec))
--                 xs
--         sourceList ys
--         applyV4SeparableFilterComplexLabeledArrayConduit parallelParams filters)



-- {-# INLINE computePhaseDifferencePair #-}

-- computePhaseDifferencePair :: [(Double, Complex Double)] -> [Double]
-- computePhaseDifferencePair [] = []
-- computePhaseDifferencePair ((0, _):xs) = computePhaseDifferencePair xs
-- computePhaseDifferencePair ((fn, xn):xs) =
--   L.concat
--     (L.map
--        (\(fm, xm) ->
--           if fm == fn || fm == 0
--             then []
--             else let mn = fromIntegral $ lcm (round fm) (round fn)
--                      (!y :+ (!z)) =
--                        (xn ** (mn / fn :+ 0)) *
--                        (conjugate $ xm ** (mn / fm :+ 0))
--                  in [y, z])
--        xs) L.++
--   computePhaseDifferencePair xs


-- {-# INLINE applyV4SeparableFilterFull #-}

-- applyV4SeparableFilterFull :: V4SeparableFilter
--                        -> VU.Vector (Complex Double)
--                        -> VU.Vector Double
-- applyV4SeparableFilterFull (V4PolarSeparableFilter (rfs, afs) filters) imgVec =
--   VU.concat .
--   L.map
--     (uncurry (VU.++) .
--      join
--        (***)
--        (\xs ->
--           let (mags, normalizedXS) =
--                 L.unzip .
--                 L.map (L.unzip . L.map normalizeComplex . applyFilter imgVec) $
--                 xs
--           in VU.fromList $
--              -- L.concat mags L.++
--              (computePhaseDifferencePair . L.concatMap (L.zip afs) $
--               normalizedXS) L.++
--              (computePhaseDifferencePair . L.concatMap (L.zip rfs) . L.transpose $
--               normalizedXS))) $
--   filters
-- applyV4SeparableFilterFull (V4CartesianSeparableFilter freqs filters) imgVec =
--   let (mags, normalizedXS) =
--         L.unzip . L.map (L.unzip . L.map normalizeComplex . applyFilter imgVec) $
--         filters
--   in VU.fromList $
--      L.concat mags L.++
--      (computePhaseDifferencePair . L.concatMap (L.zip freqs) $ normalizedXS)
-- applyV4SeparableFilterFull _ _ = error "applyV4SeparableFilterFull: filter type is not supported."
