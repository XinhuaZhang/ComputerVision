{-# LANGUAGE BangPatterns #-}
module CV.V4FilterConvolution where

import           Control.Arrow
import           Control.Concurrent.MVar          (MVar)
import           Control.Concurrent (runInBoundThread)
import           Control.DeepSeq
import           Control.Monad                    as M
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           CV.Array.LabeledArray
import           CV.Filter.CartesianGratingFilter as V4
import           CV.Filter.HyperbolicFilter       as V4
import           CV.Filter.PolarSeparableFilter   as V4 hiding (makeFilter)
import           CV.FilterExpansion
import           CV.Utility.FFT
import           CV.Utility.Parallel
import           CV.Utility.RepaArrayUtility
import           Data.Array.Repa                  as R
import           Data.Complex
import           Data.Conduit
import           Data.Conduit.List                as CL
import           Data.List                        as L
import           Data.Vector.Unboxed              as VU
import CV.V4Filter (SeparableFilterParams(..))
import Data.Vector.Storable as VS

data V4SeparableFilterParamsAxisConvolution = V4SeparableFilterParamsAxisConvolution
  { v4SeparableFilterParamsAxisConvolutionSeparableFilterRows             :: !Int
  , v4SeparableFilterParamsAxisConvolutionSeparableFilterCols             :: !Int
  , v4SeparableFilterParamsAxisConvolutionPolarSeparablePolarFactor       :: !Double
  , v4SeparableFilterParamsAxisConvolutionPolarSeparableScale             :: ![Double]
  , v4SeparableFilterParamsAxisConvolutionPolarSeparableFreq              :: ![Int]
  , v4SeparableFilterParamsAxisConvolutionPolarSeparableAngle             :: ![Double]
  , v4SeparableFilterParamsAxisConvolutionCartesianGratingScale           :: ![Double]
  , v4SeparableFilterParamsAxisConvolutionCartesianGratingFreq            :: ![Double]
  , v4SeparableFilterParamsAxisConvolutionCartesianGratingAngle           :: ![Double]
  , v4SeparableFilterParamsAxisConvolutionHyperbolicSeparableScale        :: ![Double]
  , v4SeparableFilterParamsAxisConvolutionHyperbolicSeparableUFreq        :: ![Int]
  , v4SeparableFilterParamsAxisConvolutionHyperbolicSeparableVFreq        :: ![Int]
  , v4SeparableFilterParamsAxisConvolutionHyperbolicSeparableAngle        :: !Double
  , v4SeparableFilterParamsAxisConvolutionSeparableFilterParams           :: !SeparableFilterParams
  } deriving (Show, Read)
  

generateV4SeparableFilterAxisConvolution :: V4SeparableFilterParamsAxisConvolution
                                         -> [V4SeparableFilter]
generateV4SeparableFilterAxisConvolution params =
  let polarSeparableFilterParams =
        PolarSeparableFilterParamsAxisConvolution
        { getPolarSeparableFilterAxisConvolutionRows =
          v4SeparableFilterParamsAxisConvolutionSeparableFilterRows params
        , getPolarSeparableFilterAxisConvolutionCols =
          v4SeparableFilterParamsAxisConvolutionSeparableFilterCols params
        , getPolarSeparableFilterAxisConvolutionPolarFactor =
          v4SeparableFilterParamsAxisConvolutionPolarSeparablePolarFactor params
        , getPolarSeparableFilterAxisConvolutionScale =
          v4SeparableFilterParamsAxisConvolutionPolarSeparableScale params
        , getPolarSeparableFilterAxisConvolutionFreq =
          v4SeparableFilterParamsAxisConvolutionPolarSeparableFreq params
        , getPolarSeparableFilterAxisConvolutionAngle =
          v4SeparableFilterParamsAxisConvolutionPolarSeparableAngle params
        }
      psf =
        getFilterVectors
          (makeFilter
             (PolarSeparableFilter polarSeparableFilterParams Null :: PolarSeparableFilterExpansionAxisConvolution)
             (0, 0))
  in case v4SeparableFilterParamsAxisConvolutionSeparableFilterParams params of
       P -> [psf]
       _ ->
         error "generateV4SeparableFilterWithCenterAxisConvolution: undefined."



{-# INLINE fourierTransformFilter #-}

fourierTransformFilter :: FFTW ->  (Int, Int) -> V4SeparableFilter -> IO V4SeparableFilterConvolution
fourierTransformFilter fftw (rows, cols) (V4PolarSeparableFilterAxis freqs !vecs) =
  fmap (V4PolarSeparableFilterConvolutionAxis (rows, cols) freqs) .
  M.mapM (M.mapM (dft2d fftw rows cols . VU.convert)) $
  vecs
fourierTransformFilter fftw (rows, cols) (FourierMellinTransform freqs vecs) =
  fmap (FourierMellinTransformConvolution (rows, cols) freqs) .
  M.mapM (M.mapM (M.mapM (dft2d fftw rows cols . VU.convert))) $
  vecs
fourierTransformFilter _ _ _ =
  error "fourierTransform: filter type is not supported."

{-# INLINE applyFilterConvolution #-}

applyFilterConvolution
  :: FFTW
  -> (Int, Int)
  -> Int
  -> VS.Vector (Complex Double)
  -> [VS.Vector (Complex Double)]
  -> [VS.Vector (Complex Double)]
  -> IO [VU.Vector (Complex Double)]
applyFilterConvolution fftw (rows, cols) downsampleFactor imgVec gFilters pFilters = do
  imgVecF <- dft2d fftw rows cols imgVec
  if downsampleFactor == 1
    then M.mapM
           (fmap VS.convert . idft2d fftw rows cols . VS.zipWith (*) imgVecF)
           pFilters
    else M.mapM
           (fmap
              (toUnboxed .
               computeS .
               downsample [downsampleFactor, downsampleFactor] .
               fromListUnboxed (Z :. rows :. cols) . VS.toList) .
            idft2d fftw rows cols . VS.zipWith (*) imgVecF)
           pFilters
-- mags <-
--   M.mapM
--     (fmap (liftArray (\x' -> magnitude x' :+ 0)) .
--      idftN fftw [0, 1] .
--      listArray ((0, 0), (rows - 1, cols - 1)) . VU.toList . VU.zipWith (*) imgVecF)
--     pFilters
-- fourierMags <-
--   M.mapM (fmap (VU.fromListN (rows * cols) . elems) . dftN fftw [0, 1]) mags
-- if downsampleFactor == 1
--   then fmap L.concat .
--        M.mapM
--          (\img ->
--              M.mapM
--                (fmap (VU.fromListN (rows * cols) . elems) .
--                 idftN fftw [0, 1] .
--                 listArray ((0, 0), (rows - 1, cols - 1)) .
--                 VU.toList . VU.zipWith (*) img)
--                gFilters) $
--        fourierMags
--   else fmap L.concat .
--        M.mapM
--          (\img ->
--              M.mapM
--                (fmap
--                   (toUnboxed .
--                    computeS .
--                    downsample [downsampleFactor, downsampleFactor] .
--                    fromListUnboxed (Z :. rows :. cols) . elems) .
--                 idftN fftw [0, 1] .
--                 listArray ((0, 0), (rows - 1, cols - 1)) .
--                 VU.toList . VU.zipWith (*) img)
--                gFilters) $
--        fourierMags
  


{-# INLINE applyV4FilterConvolution #-}

applyV4FilterConvolution
  :: FFTW
  -> V4SeparableFilterConvolution
  -> Int
  -> [VS.Vector (Complex Double)]
  -> [VS.Vector (Complex Double)]
  -> IO [V4SeparableFilteredImageConvolution]
applyV4FilterConvolution fftw (V4PolarSeparableFilterConvolutionAxis (rows, cols) freqs filters) downsampleFactor gFilters =
  M.mapM
    (\imgVec ->
        fmap (V4PolarSeparableFilteredImageConvolutionAxis (rows, cols) freqs) .
        M.mapM (applyFilterConvolution fftw (rows, cols) downsampleFactor imgVec gFilters) $
        filters)
applyV4FilterConvolution fftw (FourierMellinTransformConvolution (rows, cols) freqs filters) downsampleFactor gFilters =
  M.mapM
    (\imgVec ->
        fmap (FourierMellinTransformFilteredImageConvolution (rows, cols) freqs) .
        M.mapM
          (M.mapM
             (applyFilterConvolution
                fftw
                (rows, cols)
                downsampleFactor
                imgVec
                gFilters)) $
        filters)


{-# INLINE calculateV4SeparableFilterConvolutionFeature #-}

calculateV4SeparableFilterConvolutionFeature :: V4SeparableFilteredImageConvolution
                                             -> VU.Vector Double
calculateV4SeparableFilterConvolutionFeature (V4PolarSeparableFilteredImageConvolutionAxis (rows, cols) freqs filteredImgs) =
  VU.fromList .
  L.concatMap
    (\filteredImg ->
        let magnitudeImg = L.map (VU.map magnitude) filteredImg
            -- idx = findGlobalMaximaSum magnitudeImg
            -- idx = ((div rows 2) - 1) * cols + div cols 2
            -- mag = L.map (VU.! idx) magnitudeImg
            -- phaseDiff =
            --   computePhaseDifference (rows, cols) $
            --   L.zip3 freqs filteredImg magnitudeImg
            -- mag -- L.++
            -- (weightedJRP $ L.zip3 freqs filteredImg magnitudeImg)
            -- L.++
            -- L.++  (weightedMRM $ L.zip3 freqs filteredImg magnitudeImg)
            -- (weightedMagnitude $ L.zip3 freqs filteredImg magnitudeImg)
        in (computeCenterOfMass (rows, cols) $ L.zip3 freqs filteredImg magnitudeImg)) $
  filteredImgs


{-# INLINE findGlobalMaximaMultiply #-}

findGlobalMaximaMultiply :: VU.Vector Double -> VU.Vector Double -> Int
findGlobalMaximaMultiply vec1 vec2 = VU.maxIndex $ VU.zipWith (*) vec1 vec2

{-# INLINE findGlobalMaximaMultiply1 #-}

findGlobalMaximaMultiply1 :: VU.Vector Double -> VU.Vector Double -> [Int]
findGlobalMaximaMultiply1 vec1 vec2 =
  L.map fst . L.take 10 . L.reverse . L.sortOn snd . L.zip [0 ..] . VU.toList $
  VU.zipWith (*) vec1 vec2

{-# INLINE findGlobalMaximaSum #-}

findGlobalMaximaSum :: [VU.Vector Double] -> Int
findGlobalMaximaSum = VU.maxIndex . L.foldl1' (VU.zipWith (+))

{-# INLINE findGlobalMaximaCenterOfMass #-}

findGlobalMaximaCenterOfMass :: (Int, Int)
                             -> VU.Vector Double
                             -> VU.Vector Double
                             -> (Int, Int)
findGlobalMaximaCenterOfMass (rows, cols) vec1 vec2
  | s == 0 = error $ "findGlobalMaximaCenterOfMass: 0 " L.++ show (VU.sum vec1) L.++ " " L.++ show (VU.sum vec2)
  | otherwise =
    join (***) (\x -> round $ x / VU.sum vec) .
    L.foldl' (\(s1, s2) ((i, j), v) -> (s1 + i * v, s2 + j * v)) (0, 0) .
    L.zip
      [ (fromIntegral i, fromIntegral j)
      | i <- [1 .. rows]
      , j <- [1 .. cols] ] .
    VU.toList $
    VU.zipWith (*) vec1 vec2
  where
    vec = VU.zipWith (*) vec1 vec2
    s = VU.sum vec


{-# INLINE findGlobalMaximaCenterOfMass1 #-}

findGlobalMaximaCenterOfMass1 :: (Int, Int)
                             -> VU.Vector Double
                             -> (Int, Int)
findGlobalMaximaCenterOfMass1 (rows, cols) vec
  | s == 0 = error $ "findGlobalMaximaCenterOfMass1: 0 " L.++ show (VU.sum vec)
  | otherwise =
    join (***) (\x -> round $ x / VU.sum vec) .
    L.foldl' (\(s1, s2) ((i, j), v) -> (s1 + i * v, s2 + j * v)) (0, 0) .
    L.zip
      [ (fromIntegral i, fromIntegral j)
      | i <- [1 .. rows]
      , j <- [1 .. cols] ] .
    VU.toList $
    vec
  where
    s = VU.sum vec

computeCenterOfMass
  :: (Int, Int)
  -> [(Double, VU.Vector (Complex Double), VU.Vector Double)]
  -> [Double]
computeCenterOfMass (rows, cols) xs =
  let centers = L.map (findGlobalMaximaCenterOfMass1 (rows, cols)) ys
      idx = L.map (\(a, b) -> (a - 1) * cols + b - 1) centers
  in L.zipWith (VU.!) ys idx
  where
    (_, _, ys) = L.unzip3 xs

{-# INLINE phaseDifference #-}

phaseDifference
  :: Int
  -> VU.Vector (Complex Double)
  -> Double
  -> VU.Vector (Complex Double)
  -> Double
  -> [Double]
phaseDifference idx vec1 fn vec2 fm
  | idx < 0 || idx >= VU.length vec1 || idx >= VU.length vec2 =
    error $
    "phaseDifference: index is out of boundary (" L.++ show idx L.++ "," L.++
    show (VU.length vec1) L.++
    "," L.++
    show (VU.length vec2) L.++
    ")"
  | signum fn == signum fm =
    let (!y :+ (!z)) =
          (xn ** (mn / abs fn :+ 0)) * conjugate (xm ** (mn / abs fm :+ 0))
    in [mag,y,z]
  | otherwise =
    let (!y :+ (!z)) = (xn ** (mn / abs fn :+ 0)) * (xm ** (mn / abs fm :+ 0))
    in [mag,y,z]
  where
    xn = (vec1 VU.! idx) / (mag1 :+ 0)
    xm = (vec2 VU.! idx) / (mag2 :+ 0)
    mn = fromIntegral $ lcm (round . abs $ fm) (round . abs $ fn) :: Double
    mag1 = magnitude (vec1 VU.! idx)
    mag2 = magnitude (vec2 VU.! idx)
    mag = min (mag1 / mag2) (mag2 / mag1)


{-# INLINE computePhaseDifference #-}

computePhaseDifference :: (Int,Int) ->  [(Double, VU.Vector (Complex Double), VU.Vector Double)]
                       -> [Double]
computePhaseDifference _ [] = []
computePhaseDifference dim ((0, _, _):xs) = computePhaseDifference dim xs
computePhaseDifference (rows,cols) ((fn, xnC, xnM):xs) =
  L.concatMap
    (\(fm, xmC, xmM) ->
        let -- idxs = findGlobalMaximaMultiply1 xnM xmM
            --idxs = [63 * 128 + 64]
            -- rows = 128
            -- cols = 128
            (a,b) = findGlobalMaximaCenterOfMass (rows,cols) xnM xmM
            idxs = (a-1)*cols + b - 1
        in if fm == 0
              then []
              else phaseDifference idxs xnC fn xmC fm
                   -- L.foldl1' (L.zipWith (+)) .
                   -- L.map (\idx' -> phaseDifference idx' xnC fn xmC fm) $
                   -- idxs
    )
    xs L.++
  computePhaseDifference (rows,cols) xs

applyV4SeparableFilterConvolutionLabeledArrayConduit
  :: FFTW
  -> ParallelParams
  -> Int
  -> [VS.Vector (Complex Double)]
  -> [V4SeparableFilterConvolution]
  -> Conduit (LabeledArray DIM3 Double) (ResourceT IO) (Double, [V4SeparableFilteredImageConvolution])
applyV4SeparableFilterConvolutionLabeledArrayConduit fftw parallelParams downsampleFactor !gFilters !filters = do
  xs <- CL.take (batchSize parallelParams)
  unless
    (L.null xs)
    (do ys <-
          liftIO  $
          M.mapM
            (\(LabeledArray label' x) -> do
               let (Z :. channels :. _ :. _) = extent x
                   imgVecs =
                     L.map
                       (\i ->
                           VU.convert .
                           VU.map (:+ 0) . toUnboxed . computeS . R.slice x $
                           (Z :. i :. All :. All))
                       [0 .. channels - 1]
               zs <-
                 liftIO .
                 M.mapM
                   (\x' ->
                       applyV4FilterConvolution
                         fftw
                         x'
                         downsampleFactor
                         gFilters
                         imgVecs) $
                 filters
               -- !as <-
               --   liftIO $
               --   M.mapM
               --     (\(v4polarseparablefilteredimageconvolutionaxis (rows, cols) freqs vecs) ->
               --         fmap
               --           (V4PolarSeparableFilteredImageConvolutionAxis
               --              (rows, cols)
               --              freqs) .
               --         M.mapM
               --           (fmap L.concat .
               --            M.mapM
               --              (\img ->
               --                  M.mapM
               --                    (fmap (VU.fromListN (rows * cols) . elems) .
               --                     idftN fftw [0, 1] .
               --                     listArray ((0, 0), (rows - 1, cols - 1)) .
               --                     VU.toList . VU.zipWith (*) img . VU.map (\x' -> magnitude x' :+ 0))
               --                    gFilters)) $
               --         vecs) $
               --   L.concat zs
               return (fromIntegral label', L.concat zs))
            xs
        sourceList ys
        applyV4SeparableFilterConvolutionLabeledArrayConduit
          fftw
          parallelParams
          downsampleFactor
          gFilters
          filters)
-- awaitForever
--   (\(LabeledArray label' x) -> do
--      let (Z :. channels :. _ :. _) = extent x
--          imgVecs =
--            L.map
--              (\i ->
--                  VU.map (:+ 0) . toUnboxed . computeS . R.slice x $
--                  (Z :. i :. All :. All))
--              [0 .. channels - 1]
--      !ys <-
--        liftIO . runInBoundThread $
--        M.mapM
--          (\x' ->
--              applyV4FilterConvolutionIO fftw x' downsampleFactor gFilters imgVecs)
--          filters
--      yield $!! (fromIntegral label', L.concat ys))

calculateV4SeparableFilterConvolutionFeatureConduit
  :: ParallelParams
  -> Conduit (Double,[V4SeparableFilteredImageConvolution]) (ResourceT IO) (Double,VU.Vector Double)
calculateV4SeparableFilterConvolutionFeatureConduit parallelParams = do
  xs <- CL.take (batchSize parallelParams)
  unless
    (L.null xs)
    (do let (labels, filteredImgs) = L.unzip xs
        let zs =
              parMapChunk
                parallelParams
                rdeepseq
                (normalizeVec . VU.concat . L.map calculateV4SeparableFilterConvolutionFeature)
                filteredImgs
        sourceList $ L.zip labels zs
        calculateV4SeparableFilterConvolutionFeatureConduit parallelParams)

{-# INLINE normalizeVec #-}

normalizeVec :: VU.Vector Double -> VU.Vector Double
normalizeVec vec
  | s == 0 = VU.replicate (VU.length vec) 0
  | otherwise = VU.map (/ s) vec
  where
    s = sqrt . VU.sum . VU.map (^ (2 :: Int)) $ vec


{-# INLINE jrp #-}

jrp :: Complex Double -> Double -> Complex Double -> Double -> Double
jrp xn fn xm fm
  | signum fn == signum fm =
    let y =
          (xn ** (mn / abs fn :+ 0)) * conjugate (xm ** (mn / abs fm :+ 0))
    in phase y
  | otherwise =
    let y = (xn ** (mn / abs fn :+ 0)) * (xm ** (mn / abs fm :+ 0))
    in phase y
  where
    mn = fromIntegral $ lcm (round . abs $ fm) (round . abs $ fn) :: Double



{-# INLINE mrm #-}

mrm :: Double -> Double -> Double
mrm xn xm = min (xn / xm) (xm / xn)

{-# INLINE weightedSum #-}

weightedSum :: VU.Vector Double -> VU.Vector Double -> Double
weightedSum magVec vec
  | s == 0 = error "weightedSum: 0 magnitude"
  | otherwise = VU.sum (VU.zipWith (*) magVec vec) / s
  where
    s = VU.sum magVec

{-# INLINE weightedJRP #-}

weightedJRP :: [(Double, VU.Vector (Complex Double), VU.Vector Double)]
            -> [Double]
weightedJRP [] = []
weightedJRP  ((0, _, _):xs) = weightedJRP xs
weightedJRP ((fn, xnC, xnM):xs) =
  L.concatMap
    (\(fm, xmC, xmM) ->
        if fm == 0
          then []
          else let mag = VU.zipWith (*) xnM xmM
                   y = VU.zipWith (\pn pm -> jrp pn fn pm fm) xnC xmC
                   z = weightedSum mag y
               in [sin z, cos z])
    xs L.++
  weightedJRP xs


{-# INLINE weightedMRM #-}

weightedMRM :: [(Double, VU.Vector (Complex Double), VU.Vector Double)]
            -> [Double]
weightedMRM [] = []
-- weightedMRM  ((0, _, _):xs) = weightedMRM xs
weightedMRM ((_fn, _xnC, xnM):xs) =
  L.concatMap
    (\(_fm, _xmC, xmM)
      -- if fm == 0
      --   then []
      --   else
       ->
        let mag = VU.zipWith (*) xnM xmM
        in [weightedSum mag $ VU.zipWith mrm xnM xmM])
    xs L.++
  weightedMRM xs

weightedMagnitude :: [(Double, VU.Vector (Complex Double), VU.Vector Double)]
                  -> [Double]
weightedMagnitude xs =
  L.zipWith
    (\c m ->
        let (a, b) = VU.unzip . VU.map (\(a' :+ b') -> (a', b')) $ c
            avgC = weightedSum m a :+ weightedSum m b
        in magnitude avgC)
    cs
    ms
  where
    (_, cs, ms) = L.unzip3 xs
