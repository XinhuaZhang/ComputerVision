{-# LANGUAGE FlexibleContexts #-}

module CV.V4Filter
  ( module V4
  , V4QuardTreeFilterParams(..)
  , V4QuardTreeFilter
  , generateV4FilterQuardTreeFilter
  , applyV4QuardTreeFilterConduit
  , applyFilterVariedSizeConduit
  , applyFilterFixedSizeConduit
  ) where

import           Control.Monad                    as M
import           Control.Monad.Trans.Resource
import           CV.Filter.CartesianGratingFilter as V4
import           CV.Filter.HyperbolicFilter       as V4
import           CV.Filter.PolarSeparableFilter   as V4 hiding (makeFilter)
import           CV.FilterExpansion               as V4
import           CV.Utility.Parallel
import           CV.Utility.RepaArrayUtility
import           Data.Array.Repa                  as R
import           Data.Complex
import           Data.Conduit
import           Data.Conduit.List                as CL
import           Data.List                        as L
import           Data.Vector.Unboxed              as VU

data V4QuardTreeFilterParams = V4FilterQuardTreeFilterParams
  { quardTreeLayer                  :: !Int
  , rows                            :: !Int
  , cols                            :: !Int
  , polarSeparableFilterScale       :: ![Double]
  , polarSeparableFilterRadialFreq  :: ![Int]
  , polarSeparableFilterAngularFreq :: ![Int]
  , polarSeparableFilterName        :: PolarSeparableFilterName
  , cartesianGratingFilterScale     :: ![Double]
  , cartesianGratingFilterFreq      :: ![Double]
  , cartesianGratingFilterAngle     :: !Double
  , hyperbolicFilterFilterScale     :: ![Double]
  , hyperbolicFilterFilterFreq      :: ![Double]
  , hyperbolicFilterFilterAngle     :: !Double
  } deriving (Show,Read)

type V4QuardTreeFilter = [[[VU.Vector (Complex Double)]]]

generateV4FilterQuardTreeFilter :: V4QuardTreeFilterParams -> V4QuardTreeFilter
generateV4FilterQuardTreeFilter params =
  L.zipWith3
    (\i psfRadialFreq psfAngleFreq ->
        let gridRows = 2 ^ i
            gridCols = gridRows
            polarSeparableFilterParams =
              PolarSeparableFilterParamsGrid
              { getPolarSeparableFilterGridRows = gridRows
              , getPolarSeparableFilterGridCols = gridCols
              , getPolarSeparableFilterRows = rows params
              , getPolarSeparableFilterCols = cols params
              , getPolarSeparableFilterDownsampleFactor = 1
              , getPolarSeparableFilterScale =
                L.map (/ (2 ^ i)) $ polarSeparableFilterScale params
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
                L.map (/ (2 ^ i)) $ cartesianGratingFilterScale params
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
                L.map (/ (2 ^ i)) $ hyperbolicFilterFilterScale params
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
    [quardTreeLayer params - 1 .. 0]
    (polarSeparableFilterRadialFreq params)
    (polarSeparableFilterAngularFreq params)
    

applyV4QuardTreeFilterConduit
  :: (R.Source s Double)
  => ParallelParams
  -> V4QuardTreeFilter
  -> Conduit (R.Array s DIM3 Double) (ResourceT IO) [[VU.Vector Double]]
applyV4QuardTreeFilterConduit parallelParams filters = do
  xs <- CL.take (batchSize parallelParams)
  unless
    (L.null xs)
    (do let ys =
              parMapChunk
                parallelParams
                rdeepseq
                (\x ->
                    let (Z :. channels :. _ :. _) = extent x
                        imgVecs =
                          L.map
                            (\i ->
                                VU.map (:+ 0) . toUnboxed . computeS . R.slice x $
                                (Z :. i :. All :. All))
                            [0 .. channels - 1]
                    in L.map (applyFilter imgVecs) filters)
                xs
        sourceList ys
        applyV4QuardTreeFilterConduit parallelParams filters)

applyFilterVariedSizeConduit
  :: (R.Source s Double)
  => ParallelParams
  -> PolarSeparableFilterParamsGrid
  -> CartesianGratingFilterParams
  -> HyperbolicFilterParams
  -> Conduit (R.Array s DIM3 Double) (ResourceT IO) [VU.Vector Double]
applyFilterVariedSizeConduit parallelParams polarFilterParams cartesianGratingFilterParams hyperbolicFilterParams = do
  xs <- CL.take (batchSize parallelParams)
  unless
    (L.null xs)
    (do let ys =
              parMapChunk
                parallelParams
                rdeepseq
                (\x ->
                    let (Z :. channels :. rows :. cols) = extent x
                        psf =
                          makeFilter . changeSizeParameter rows cols $
                          PolarSeparableFilter polarFilterParams [] :: PolarSeparableFilterExpansion
                        cgf =
                          makeFilter . changeSizeParameter rows cols $
                          CartesianGratingFilter cartesianGratingFilterParams [] :: CartesianGratingFilter
                        hf =
                          makeFilter . changeSizeParameter rows cols $
                          HyperbolicFilter hyperbolicFilterParams [] :: HyperbolicFilter
                        psfVecs = getFilterVectors psf
                        cgfVecs = getFilterVectors cgf
                        hfVecs = getFilterVectors hf
                        filterVecsList =
                          L.zipWith3
                            (\a b c -> a L.++ b L.++ c)
                            psfVecs
                            cgfVecs
                            hfVecs
                        downSampleFactor =
                          getPolarSeparableFilterDownsampleFactor polarFilterParams
                        img =
                          downsample [downSampleFactor, downSampleFactor, 1] x
                        imgVecs =
                          L.map
                            (\i ->
                                VU.map (:+ 0) . toUnboxed . computeS . R.slice img $
                                (Z :. i :. All :. All))
                            [0 .. channels - 1]
                    in applyFilter imgVecs filterVecsList)
                xs
        sourceList ys
        applyFilterVariedSizeConduit
          parallelParams
          polarFilterParams
          cartesianGratingFilterParams
          hyperbolicFilterParams)

applyFilterFixedSizeConduit
  :: (R.Source s Double)
  => ParallelParams
  -> Int
  -> [[VU.Vector (Complex Double)]]
  -> Conduit (R.Array s DIM3 Double) (ResourceT IO) [VU.Vector Double]
applyFilterFixedSizeConduit parallelParams downSampleFactor filterVecsList = do
  xs <- CL.take (batchSize parallelParams)
  unless
    (L.null xs)
    (do let ys =
              parMapChunk
                parallelParams
                rdeepseq
                (\x ->
                    let (Z :. channels :. _ :. _) = extent x
                        img =
                          downsample [downSampleFactor, downSampleFactor, 1] x
                        imgVecs =
                          L.map
                            (\i ->
                                VU.map (:+ 0) . toUnboxed . computeS . R.slice img $
                                (Z :. i :. All :. All))
                            [0 .. channels - 1]
                    in applyFilter imgVecs filterVecsList)
                xs
        sourceList ys
        applyFilterFixedSizeConduit parallelParams downSampleFactor filterVecsList)

{-# INLINE normalizeVec #-}

normalizeVec :: VU.Vector Double -> VU.Vector Double
normalizeVec vec = VU.map (/ s) vec
  where
    s = sqrt . VU.sum . VU.map (^ (2 :: Int)) $ vec

{-# INLINE complexVec2RealVec #-}

complexVec2RealVec :: VU.Vector (Complex Double) -> VU.Vector Double
complexVec2RealVec vec = a VU.++ b
  where
    (a, b) = VU.unzip . VU.map polar $ vec

{-# INLINE applyFilter #-}

applyFilter :: [VU.Vector (Complex Double)]
            -> [[VU.Vector (Complex Double)]]
            -> [VU.Vector Double]
applyFilter imgVecs =
  L.map
    (\filterVecs ->
        normalizeVec .
        complexVec2RealVec .
        VU.fromList .
        L.concatMap
          (\imgVec -> L.map (VU.sum . VU.zipWith (*) imgVec) filterVecs) $
        imgVecs)
