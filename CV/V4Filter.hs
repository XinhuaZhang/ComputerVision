{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}

module CV.V4Filter
  ( module V4
  , SeparableFilterParams(..)
  , V4SeparableFilterParams
  , generateV4SeparableFilter
  ) where

import           Control.Monad                    as M
import           Control.Monad.Trans.Resource
import           CV.Array.LabeledArray
import           CV.Filter.CartesianGratingFilter as V4
import           CV.Filter.GaussianFilter
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
import Control.Arrow

data SeparableFilterParams
  = P
  | C
  | H
  | PC
  | PH
  | CH
  | PCH
  deriving (Show, Read)

data V4SeparableFilterParams = V4SeparableFilterParams
  { separableFilterRows          :: !Int
  , separableFilterCols          :: !Int
  , polarSeparableScale          :: ![Double]
  , polarSeparableRadialFreq     :: ![Int]
  , polarSeparableAngularFreq    :: ![Int]
  -- , cartesianSeparableScale      :: ![Double]
  -- , cartesianSeparableXFreq      :: ![Int]
  -- , cartesianSeparableYFreq      :: ![Int]
  -- , hyperbolicSeparableScale     :: ![Double]
  -- , hyperbolicSeparableUFreq     :: ![Int]
  -- , hyperbolicSeparableVFreq     :: ![Int]
  -- , hyperbolicSeparableAngle     :: !Double
  , separableFilterParams        :: !SeparableFilterParams
  } deriving (Show, Read)

<<<<<<< HEAD
type V4QuadTreeFilter = [[[VU.Vector (Complex Double)]]]
type V4QuadTreeSeparableFilter = [[[([[VU.Vector (Complex Double)]], [[VU.Vector (Complex Double)]])]]]

generateV4FilterQuadTreeFilter :: V4QuadTreeFilterParams -> V4QuadTreeFilter
generateV4FilterQuadTreeFilter params =
  L.zipWith3
    (\i psfRadialFreq psfAngleFreq ->
        let gridRows = 2 ^ i
            gridCols = gridRows
            j = i
            polarSeparableFilterParams =
              PolarSeparableFilterParamsGrid
              { getPolarSeparableFilterGridRows = gridRows
              , getPolarSeparableFilterGridCols = gridCols
              , getPolarSeparableFilterRows = rows params
              , getPolarSeparableFilterCols = cols params
              , getPolarSeparableFilterDownsampleFactor = 1
              , getPolarSeparableFilterScale =
                L.map (/ ((sqrt 2) ^ j)) $ polarSeparableFilterScale params
              , getPolarSeparableFilterRadialFreq = [0 .. psfRadialFreq - 1]
              , getPolarSeparableFilterAngularFreq = [0 .. psfAngleFreq - 1]
              , getPolarSeparableFilterName = polarSeparableFilterName params
              }
            cgfAngle = cartesianGratingFilterAngle params
            cartesianGratingFilterParams =
              CartesianGratingFilterParams
              { getCartesianGratingFilterGridRows = gridRows
              , getCartesianGratingFilterGridCols = gridCols
              , getCartesianGratingFilterRows = rows params
              , getCartesianGratingFilterCols = cols params
              , getCartesianGratingFilterDownsampleFactor = 1
              , getCartesianGratingFilterScale =
                L.map (/ ((sqrt 2) ^ j)) $ cartesianGratingFilterScale params
              , getCartesianGratingFilterFreq = cartesianGratingFilterFreq params
              , getCartesianGratingFilterAngle = [0,cgfAngle .. 180 - cgfAngle]
              }
            hfAngle = hyperbolicFilterFilterAngle params
            hyperbolicFilterParams =
              HyperbolicFilterParams
              { getHyperbolicFilterGridRows = gridRows
              , getHyperbolicFilterGridCols = gridCols
              , getHyperbolicFilterRows = rows params
              , getHyperbolicFilterCols = cols params
              , getHyperbolicFilterDownsampleFactor = 1
              , getHyperbolicFilterScale =
                L.map (/ ((sqrt 2) ^ j)) $ hyperbolicFilterFilterScale params
              , getHyperbolicFilterFreq = hyperbolicFilterFilterFreq params
              , getHyperbolicFilterAngle = [0,hfAngle .. 90 - hfAngle]
              }
            psf =
              getFilterVectors
                (makeFilter $ PolarSeparableFilter polarSeparableFilterParams [] :: PolarSeparableFilterExpansion)
            cgf =
              getFilterVectors
                (makeFilter $
                 CartesianGratingFilter cartesianGratingFilterParams [] :: CartesianGratingFilter)
            hf =
              getFilterVectors
                (makeFilter $ HyperbolicFilter hyperbolicFilterParams [] :: HyperbolicFilter)
        in L.zipWith3 (\a b c -> L.concat [a, b, c]) psf cgf hf)
    [0..quadTreeLayer params - 1]
    (polarSeparableFilterRadialFreq params)
    (polarSeparableFilterAngularFreq params)


generateV4SeparableFilterQuadTreeFilter :: V4QuadTreeSeparableFilterParams -> V4QuadTreeFilter
generateV4SeparableFilterQuadTreeFilter params =
  L.zipWith3
    (\i psfRadialFreq psfAngleFreq ->
       let gridRows = 2 ^ i
           gridCols = gridRows
           polarSeparableFilterParams =
             PolarSeparableFilterParamsGrid
             { getPolarSeparableFilterGridRows = gridRows
             , getPolarSeparableFilterGridCols = gridCols
             , getPolarSeparableFilterRows = separableFilterRows params
             , getPolarSeparableFilterCols = separableFilterCols params
             , getPolarSeparableFilterDownsampleFactor = 1
             , getPolarSeparableFilterScale =
                 L.map (/ (sqrt 2 ^ i)) $ polarSeparableScale params
             , getPolarSeparableFilterRadialFreq =  L.take psfRadialFreq [0,4..]  --[0,4 .. psfRadialFreq - 0]
             , getPolarSeparableFilterAngularFreq = [0 .. psfAngleFreq - 1]
             , getPolarSeparableFilterName = polarSeparableName params
             }
           cartesianSeparableFilterParams =
             CartesianSeparableFilterParams
             { getCartesianSeparableFilterGridRows = gridRows
             , getCartesianSeparableFilterGridCols = gridCols
             , getCartesianSeparableFilterRows = separableFilterRows params
             , getCartesianSeparableFilterCols = separableFilterCols params
             , getCartesianSeparableFilterDownsampleFactor = 1
             , getCartesianSeparableFilterScale =
                 L.map (/ (sqrt 2 ^ i)) $ cartesianSeparableScale params
             , getCartesianSeparableFilterXFreq = cartesianSeparableXFreq params
             , getCartesianSeparableFilterYFreq = cartesianSeparableYFreq params
             }
           hfAngle = hyperbolicSeparableAngle params
           hyperbolicSeparableFilterParams =
             HyperbolicSeparableFilterParams
             { getHyperbolicSeparableFilterGridRows = gridRows
             , getHyperbolicSeparableFilterGridCols = gridCols
             , getHyperbolicSeparableFilterRows = separableFilterRows params
             , getHyperbolicSeparableFilterCols = separableFilterCols params
             , getHyperbolicSeparableFilterDownsampleFactor = 1
             , getHyperbolicSeparableFilterScale =
                 L.map (/ (sqrt 2 ^ i)) $ hyperbolicSeparableScale params
             , getHyperbolicSeparableFilterUFreq =
                 hyperbolicSeparableUFreq params
             , getHyperbolicSeparableFilterVFreq =
                 hyperbolicSeparableVFreq params
             , getHyperbolicSeparableFilterAngle = [0,hfAngle .. 90 - hfAngle]
             }
           psf =
             getFilterVectors
               (makeFilter $ PolarSeparableFilter polarSeparableFilterParams [] :: PolarSeparableFilterExpansion)
           cgf =
             getFilterVectors
               (makeFilter $
                CartesianSeparableFilter cartesianSeparableFilterParams [] :: CartesianSeparableFilter)
           hf =
             getFilterVectors
               (makeFilter $
                HyperbolicSeparableFilter hyperbolicSeparableFilterParams [] :: HyperbolicSeparableFilter)
       in case separableFilterParams params of
            P -> psf
            C -> cgf
            H -> hf
            PC -> L.zipWith (\a b -> L.concat [a, b]) psf cgf
            PH -> L.zipWith (\a b -> L.concat [a, b]) psf hf
            CH -> L.zipWith (\a b -> L.concat [a, b]) cgf hf
            PCH -> L.zipWith3 (\a b c -> L.concat [a, b, c]) psf cgf hf)
    [0 .. separableFilterQuadTreeLayer params - 1]
    (polarSeparableRadialFreq params)
    (polarSeparableAngularFreq params)
=======
generateV4SeparableFilter :: V4SeparableFilterParams -> [V4SeparableFilter]
generateV4SeparableFilter params =
  let polarSeparableFilterParams =
        PolarSeparableFilterParamsGrid
        { getPolarSeparableFilterRows = separableFilterRows params
        , getPolarSeparableFilterCols = separableFilterCols params
        , getPolarSeparableFilterScale = polarSeparableScale params
        , getPolarSeparableFilterRadialFreq = polarSeparableRadialFreq params
        , getPolarSeparableFilterAngularFreq = polarSeparableAngularFreq params
        }
      -- cartesianSeparableFilterParams =
      --   CartesianSeparableFilterParams
      --   { getCartesianSeparableFilterRows = separableFilterRows params
      --   , getCartesianSeparableFilterCols = separableFilterCols params
      --   , getCartesianSeparableFilterScale = cartesianSeparableScale params
      --   , getCartesianSeparableFilterXFreq = cartesianSeparableXFreq params
      --   , getCartesianSeparableFilterYFreq = cartesianSeparableYFreq params
      --   }
      -- hfAngle = hyperbolicSeparableAngle params
      -- hyperbolicSeparableFilterParams =
      --   HyperbolicSeparableFilterParams
      --   { getHyperbolicSeparableFilterGridRows = gridRows
      --   , getHyperbolicSeparableFilterGridCols = gridCols
      --   , getHyperbolicSeparableFilterRows = separableFilterRows params
      --   , getHyperbolicSeparableFilterCols = separableFilterCols params
      --   , getHyperbolicSeparableFilterDownsampleFactor = 1
      --   , getHyperbolicSeparableFilterScale =
      --     L.map (/ (sqrt 2 ^ i)) $ hyperbolicSeparableScale params
      --   , getHyperbolicSeparableFilterUFreq = hyperbolicSeparableUFreq params
      --   , getHyperbolicSeparableFilterVFreq = hyperbolicSeparableVFreq params
      --   , getHyperbolicSeparableFilterAngle = [0,hfAngle .. 90 - hfAngle]
      --   }
      psf =
        getFilterVectors
          (makeFilter $ PolarSeparableFilter polarSeparableFilterParams Null :: PolarSeparableFilterExpansion)
      -- cgf =
      --   getFilterVectors
      --     (makeFilter $
      --      CartesianSeparableFilter cartesianSeparableFilterParams [] :: CartesianSeparableFilter)
      -- hf =
      --   getFilterVectors
      --     (makeFilter $
      --      HyperbolicSeparableFilter hyperbolicSeparableFilterParams [] :: HyperbolicSeparableFilter)
  in case separableFilterParams params of
       P -> [psf]
       -- C -> cgf
       -- H -> hf
       -- PC -> [psf, cgf]
       -- PH -> [psf, hf]
       -- CH -> [cgf, hf]
       -- PCH -> [psf, cgf, hf]
>>>>>>> 4f672415c8b3fd7d3d2c865b4192deb9bc9e6cfa

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

<<<<<<< HEAD
{-# INLINE applyFilter #-}

applyFilter :: [VU.Vector (Complex Double)]
            -> [[VU.Vector (Complex Double)]]
            -> [VU.Vector Double]
applyFilter imgVecs =
  L.map
    (\filterVecs ->
       normalizeVec .
       complexVec2RealVec . VU.fromList .
       -- VU.concat .
       -- L.map (normalizeVec . complexVec2RealVec . VU.fromList) .
       L.concatMap (\imgVec -> L.map (VU.sum . VU.zipWith (*) imgVec) filterVecs) $
       imgVecs)
=======
-- {-# INLINE applyFilter #-}
>>>>>>> 4f672415c8b3fd7d3d2c865b4192deb9bc9e6cfa

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
computePhaseDifference [] [] = []
computePhaseDifference (fn:fs) (xn:xs) =
  L.concat
    (L.zipWith
       (\fm xm ->
           let (!y :+ (!z)) = (xn ** (fm :+ 0)) * (xm ** (fn :+ 0))
           in [y, z])
       fs
       xs) L.++
  computePhaseDifference fs xs

{-# INLINE applyFilter #-}

applyFilter :: VU.Vector (Complex Double)
            -> [VU.Vector (Complex Double)]
            -> [Complex Double]
applyFilter imgVec = L.map (VU.sum . VU.zipWith (*) imgVec)


{-# INLINE applyV4SeparableFilter #-}

applyV4SeparableFilter :: V4SeparableFilter
                       -> VU.Vector (Complex Double)
                       -> VU.Vector Double
applyV4SeparableFilter (V4PolarSeparableFilter (rfs, afs) filters) imgVec =
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
              (L.concatMap (computePhaseDifference rfs) . L.transpose $ normalizedXS))) $
  filters
applyV4SeparableFilter (V4CartesianSeparableFilter freqs filters) imgVec =
  let (mags, normalizedXS) =
        L.unzip . L.map (L.unzip . L.map normalizeComplex . applyFilter imgVec) $
        filters
  in VU.fromList $
     L.concat mags L.++ L.concatMap (computePhaseDifference freqs) normalizedXS
applyV4SeparableFilter _ _ = error "applyV4SeparableFilter: filter type is not supported."  
