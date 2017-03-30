{-# LANGUAGE FlexibleContexts #-}

module CV.V4Filter
  ( module V4
  , V4QuadTreeFilterParams(..)
  , V4QuadTreeSeparableFilterParams(..)
  , V4QuadTreeFilter
  , generateV4FilterQuadTreeFilter
  , generateV4SeparableFilterQuadTreeFilter
  , applyV4QuadTreeFilterLabeledArrayConduit
  , applyV4QuadTreeFilterConduit
  , applyFilterVariedSizeConduit
  , applyFilterFixedSizeConduit
  , makeV4Filter
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

data V4QuadTreeFilterParams = V4QuadTreeFilterParams
  { quadTreeLayer                   :: !Int
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


data V4QuadTreeSeparableFilterParams = V4QuadTreeSeparableFilterParams
  { separableFilterQuadTreeLayer :: !Int
  , separableFilterRows          :: !Int
  , separableFilterCols          :: !Int
  , polarSeparableScale          :: ![Double]
  , polarSeparableRadialFreq     :: ![Int]
  , polarSeparableAngularFreq    :: ![Int]
  , polarSeparableName           :: PolarSeparableFilterName
  , cartesianSeparableScale      :: ![Double]
  , cartesianSeparableXFreq      :: ![Int]
  , cartesianSeparableYFreq      :: ![Int]
  , hyperbolicSeparableScale     :: ![Double]
  , hyperbolicSeparableUFreq     :: ![Int]
  , hyperbolicSeparableVFreq     :: ![Int]
  , hyperbolicSeparableAngle     :: !Double
  } deriving (Show, Read)

type V4QuadTreeFilter = [[[VU.Vector (Complex Double)]]]

generateV4FilterQuadTreeFilter :: V4QuadTreeFilterParams -> V4QuadTreeFilter
generateV4FilterQuadTreeFilter params =
  L.zipWith3
    (\i psfRadialFreq psfAngleFreq ->
        let gridRows = 2 ^ i
            gridCols = gridRows
            j = if i > i
                   then 2
                   else i
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
              , getPolarSeparableFilterRadialFreq = [0 .. psfRadialFreq - 1]
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
              , getHyperbolicSeparableFilterUFreq = hyperbolicSeparableUFreq params
              , getHyperbolicSeparableFilterVFreq = hyperbolicSeparableVFreq params
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
        in L.zipWith3 (\a b c -> L.concat [a,b,c]) psf cgf hf)
    [0 .. separableFilterQuadTreeLayer params - 1]
    (polarSeparableRadialFreq params)
    (polarSeparableAngularFreq params)


applyV4QuadTreeFilterConduit
  :: (R.Source s Double)
  => ParallelParams
  -> V4QuadTreeFilter
  -> Conduit (R.Array s DIM3 Double) (ResourceT IO) [[VU.Vector Double]]
applyV4QuadTreeFilterConduit parallelParams filters = do
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
        applyV4QuadTreeFilterConduit parallelParams filters)
        

applyV4QuadTreeFilterLabeledArrayConduit
  :: ParallelParams
  -> V4QuadTreeFilter
  -> Conduit (LabeledArray DIM3 Double) (ResourceT IO) (Double, [[VU.Vector Double]])
applyV4QuadTreeFilterLabeledArrayConduit parallelParams filters = do
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
                   in (fromIntegral l, L.map (applyFilter imgVecs) filters))
                xs
        sourceList ys
        applyV4QuadTreeFilterLabeledArrayConduit parallelParams filters)

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


makeV4Filter
  :: V4QuadTreeFilterParams
  -> V4QuadTreeFilter
makeV4Filter (V4QuadTreeFilterParams numLayer r c polarScale prFreq paFreq _ _ cgFreq cgAngle _ hbFreq hbAngle) =
  L.zipWith3
    (\i psfRadialFreq psfAngleFreq ->
       let gridRows = 2 ^ i
           gridCols = gridRows
       in L.map
            (\(centerR, centerC) ->
               [ VU.fromListN
                 (r * c)
                 [ v4Function
                   s
                   angleFreq
                   radialFreq
                   hbTheta
                   hbf
                   cgTheta
                   cgf
                   (x - centerR)
                   (y - centerC)
                 | y <- [0 .. r - 1]
                 , x <- [0 .. c - 1]
                 ]
               | s <- polarScale
               , angleFreq <- [0 .. psfAngleFreq - 1]
               , radialFreq <- [0 .. psfRadialFreq - 1]
               , hbTheta <- hbRadAngle
               , hbf <- hbFreq
               , cgTheta <- cgRadAngle
               , cgf <- cgFreq
               ]) $
          grid2D (r, c) (gridRows, gridCols))
    [0 .. numLayer - 1]
    prFreq
    paFreq
  where
    hbRadAngle = L.map deg2Rad [0,hbAngle .. 90 - hbAngle]
    cgRadAngle = L.map deg2Rad [0,cgAngle .. 180 - cgAngle]

{-# INLINE v4Function #-}               

v4Function
  :: Double
  -> Int
  -> Int
  -> Double
  -> Double
  -> Double
  -> Double
  -> Int
  -> Int
  -> Complex Double
v4Function scale angleFreq radialFreq hbTheta hbFreq cgTheta cgFreq x y =
  real2Complex (gaussian2D' angleFreq radialFreq scale x y) *
  ejx (fromIntegral angleFreq * polarTheta) *
  ejx (fromIntegral radialFreq * polarR * (2 * pi) / (4 * scale)) *
  ejx (hbFreq * (sqrt . abs $! (u * v))) *
  ejx (cgFreq * u)
  where
    polarTheta = angleFunctionRad (fromIntegral x) (fromIntegral y)
    polarR = sqrt . fromIntegral $ x ^ (2 :: Int) + y ^ (2 :: Int)
    c = cos hbTheta
    s = sin hbTheta
    u = fromIntegral x * c - fromIntegral y * s
    v = fromIntegral x * s + fromIntegral y * c
