{-# LANGUAGE BangPatterns #-}
module CV.V4FilterConvolution where

import           Control.Arrow
import           Control.Concurrent.MVar          (MVar)
import           Control.DeepSeq
import           Control.Monad                    as M
import           Control.Monad.Parallel                    as MP
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
import qualified Data.Array                       as Arr
import           Data.Array.CArray                as CA
import           Data.Array.Repa                  as R
import           Data.Complex
import           Data.Conduit
import           Data.Conduit.List                as CL
import qualified Data.Image                       as UNM
import           Data.List                        as L
import           Data.Vector.Unboxed              as VU
import qualified Math.FFT                         as MF


{-# INLINE fourierTransformFilter #-}

fourierTransformFilter :: MVar () ->  (Int, Int) -> V4SeparableFilter -> IO V4SeparableFilterConvolution
fourierTransformFilter lock (rows, cols) (V4PolarSeparableFilterAxis freqs !vecs) =
  fmap (V4PolarSeparableFilterConvolutionAxis (rows, cols) freqs) .
  M.mapM
    (MP.mapM
        (fmap (VU.fromListN (rows * cols) . elems) .
         dftN lock [0, 1] . CA.listArray ((0, 0), (rows - 1, cols - 1)) . VU.toList)) $
  vecs
fourierTransformFilter lock (rows, cols) (FourierMellinTransform freqs vecs) =
  fmap (FourierMellinTransformConvolution (rows, cols) freqs) .
  M.mapM
    (M.mapM
       (MP.mapM
           (fmap (VU.fromListN (rows * cols) . elems) .
            dftN lock [0, 1] . CA.listArray ((0, 0), (rows - 1, cols - 1)) . VU.toList))) $
  vecs
fourierTransformFilter _ _ _ =
  error "fourierTransform: filter type is not supported."


{-# INLINE fourierTransformFilterP #-}

fourierTransformFilterP :: (Int, Int) -> V4SeparableFilter -> V4SeparableFilterConvolution
fourierTransformFilterP (rows, cols) (V4PolarSeparableFilterAxis freqs !vecs) =
  V4PolarSeparableFilterConvolutionAxis (rows, cols) freqs .
  parMap
    rdeepseq
    (L.map
       ((VU.fromListN (rows * cols) . elems) .
        MF.dftN [0, 1] . CA.listArray ((0, 0), (rows - 1, cols - 1)) . VU.toList)) $
  vecs
fourierTransformFilterP (rows, cols) (FourierMellinTransform freqs vecs) =
  FourierMellinTransformConvolution (rows, cols) freqs .
  parMap
    rdeepseq
    (L.map
       (L.map
          ((VU.fromListN (rows * cols) . elems) .
           MF.dftN [0, 1] . CA.listArray ((0, 0), (rows - 1, cols - 1)) . VU.toList))) $
  vecs
fourierTransformFilterP _ _ =
  error "fourierTransform: filter type is not supported."


{-# INLINE fourierTransformFilterUNM #-}

fourierTransformFilterUNM :: (Int, Int) -> V4SeparableFilter -> V4SeparableFilterConvolution
fourierTransformFilterUNM (rows, cols) (V4PolarSeparableFilterAxis freqs !vecs) =
  V4PolarSeparableFilterConvolutionAxis (rows, cols) freqs .
  parMap
    rdeepseq
    (L.map
       (\x ->
           (VU.fromListN (rows * cols) . UNM.pixelList) . UNM.fft $
           (UNM.arrayToImage .
            Arr.listArray ((0, 0), (rows - 1, cols - 1)) . VU.toList $
            x :: UNM.ComplexImage))) $
  vecs
fourierTransformFilterUNM (rows, cols) (FourierMellinTransform freqs vecs) =
  FourierMellinTransformConvolution (rows, cols) freqs .
  parMap
    rdeepseq
    (L.map
       (L.map
          ((VU.fromListN (rows * cols) . elems) .
           MF.dftN [0, 1] . CA.listArray ((0, 0), (rows - 1, cols - 1)) . VU.toList))) $
  vecs
fourierTransformFilterUNM _ _ =
  error "fourierTransform: filter type is not supported."


{-# INLINE applyFilterConvolutionIO #-}

applyFilterConvolutionIO
  :: MVar ()
  -> (Int, Int)
  -> Int
  -> VU.Vector (Complex Double)
  -> [VU.Vector (Complex Double)]
  -> [VU.Vector (Complex Double)]
  -> IO [VU.Vector (Complex Double)]
applyFilterConvolutionIO lock (rows, cols) downsampleFactor imgVec gFilters pFilters = do
  imgVecF <-
    fmap (VU.fromListN (rows * cols) . elems) .
    dftN lock [0, 1] . listArray ((0, 0), (rows - 1, cols - 1)) . VU.toList $
    imgVec
  if downsampleFactor == 1
    then MP.mapM
           (fmap (VU.fromListN (rows * cols) . CA.elems) .
            idftN lock [0, 1] .
            listArray ((0, 0), (rows - 1, cols - 1)) .
            VU.toList . VU.zipWith (*) imgVecF)
           pFilters
    else fmap L.concat .
         MP.mapM
           (\pf ->
               M.mapM
                 (fmap
                    (toUnboxed .
                     computeS .
                     downsample [downsampleFactor, downsampleFactor] .
                     fromListUnboxed (Z :. rows :. cols) . elems) .
                  idftN lock [0, 1] .
                  listArray ((0, 0), (rows - 1, cols - 1)) .
                  VU.toList . VU.zipWith3 (\a b c -> a * b * c) imgVecF pf)
                 gFilters) $
         pFilters



{-# INLINE applyFilterConvolution #-}

applyFilterConvolution
  :: (Int, Int)
  -> Int
  -> VU.Vector (Complex Double)
  -> [VU.Vector (Complex Double)]
  -> [VU.Vector (Complex Double)]
  -> [VU.Vector (Complex Double)]
applyFilterConvolution (rows, cols) downsampleFactor imgVec gFilters pFilters =
  let imgVecF =
        (VU.fromListN (rows * cols) . elems) .
        MF.dftN [0, 1] . listArray ((0, 0), (rows - 1, cols - 1)) . VU.toList $
        imgVec
      xs = L.map (VU.zipWith (*) imgVecF) pFilters
      ys = L.concatMap (\x -> L.map (VU.zipWith (*) x) gFilters) xs
  in if downsampleFactor == 1
       then L.map
              ((VU.fromListN (rows * cols) . CA.elems) .
               MF.idftN [0, 1] . listArray ((0, 0), (rows - 1, cols - 1)) . VU.toList)
              ys
       else L.map
              (toUnboxed .
               computeS .
               downsample [downsampleFactor, downsampleFactor] .
               fromListUnboxed (Z :. rows :. cols) .
               elems .
               MF.idftN [0, 1] . listArray ((0, 0), (rows - 1, cols - 1)) . VU.toList)
              ys

{-# INLINE applyV4FilterConvolutionIO #-}

applyV4FilterConvolutionIO
  :: MVar ()
  -> V4SeparableFilterConvolution
  -> Int
  -> [VU.Vector (Complex Double)]
  -> [VU.Vector (Complex Double)]
  -> IO [V4SeparableFilteredImageConvolution]
applyV4FilterConvolutionIO lock (V4PolarSeparableFilterConvolutionAxis (rows, cols) freqs filters) downsampleFactor gFilters =
  M.mapM
    (\imgVec ->
        fmap (V4PolarSeparableFilteredImageConvolutionAxis (rows, cols) freqs) .
        M.mapM (applyFilterConvolutionIO lock (rows, cols) downsampleFactor imgVec gFilters) $
        filters)
applyV4FilterConvolutionIO lock (FourierMellinTransformConvolution (rows, cols) freqs filters) downsampleFactor gFilters =
  M.mapM
    (\imgVec ->
        fmap (FourierMellinTransformFilteredImageConvolution (rows, cols) freqs) .
        M.mapM
          (M.mapM
             (applyFilterConvolutionIO
                lock
                (rows, cols)
                downsampleFactor
                imgVec
                gFilters)) $
        filters)


{-# INLINE applyV4FilterConvolution #-}

applyV4FilterConvolution
  :: V4SeparableFilterConvolution
  -> Int
  -> [VU.Vector (Complex Double)]
  -> [VU.Vector (Complex Double)]
  -> [V4SeparableFilteredImageConvolution]
applyV4FilterConvolution (V4PolarSeparableFilterConvolutionAxis (rows, cols) freqs filters) downsampleFactor gFilters =
  L.map
    (\imgVec ->
        V4PolarSeparableFilteredImageConvolutionAxis (rows, cols) freqs .
        L.map
          (applyFilterConvolution (rows, cols) downsampleFactor imgVec gFilters) $
        filters)
applyV4FilterConvolution (FourierMellinTransformConvolution (rows, cols) freqs filters) downsampleFactor gFilters =
  L.map
    (\imgVec ->
        FourierMellinTransformFilteredImageConvolution (rows, cols) freqs .
        L.map
          (L.map
             (applyFilterConvolution (rows, cols) downsampleFactor imgVec gFilters)) $
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
       in -- (weightedJRP $ L.zip3 freqs filteredImg magnitudeImg)
          -- L.++
          -- L.++  (weightedMRM $ L.zip3 freqs filteredImg magnitudeImg)
          -- (weightedMagnitude $ L.zip3 freqs filteredImg magnitudeImg)
          (computeCenterOfMass (rows,cols) $ L.zip3 freqs filteredImg magnitudeImg)
    ) $
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
  :: MVar ()
  -> Int
  -> [VU.Vector (Complex Double)]
  -> [V4SeparableFilterConvolution]
  -> Conduit (LabeledArray DIM3 Double) (ResourceT IO) (Double, [V4SeparableFilteredImageConvolution])
applyV4SeparableFilterConvolutionLabeledArrayConduit lock downsampleFactor gFilters filters =
  awaitForever
    (\(LabeledArray label' x) -> do
       let (Z :. channels :. _ :. _) = extent x
           imgVecs =
             L.map
               (\i ->
                   VU.map (:+ 0) . toUnboxed . computeS . R.slice x $
                   (Z :. i :. All :. All))
               [0 .. channels - 1]
       ys <-
         liftIO $
         M.mapM
           (\x' -> applyV4FilterConvolutionIO lock x' downsampleFactor imgVecs gFilters)
           filters
       yield (fromIntegral label', L.concat ys))


applyV4SeparableFilterConvolutionLabeledArrayConduitP
  :: ParallelParams
  -> Int
  -> [VU.Vector (Complex Double)]
  -> [V4SeparableFilterConvolution]
  -> Conduit (LabeledArray DIM3 Double) (ResourceT IO) (Double, [V4SeparableFilteredImageConvolution])
applyV4SeparableFilterConvolutionLabeledArrayConduitP parallelParams downsampleFactor gFilters pFilters = do
  xs <- CL.take (batchSize parallelParams)
  unless
    (L.null xs)
    (do let ys =
              parMapChunk
                parallelParams
                rdeepseq
                (\(LabeledArray label' x) ->
                    let (Z :. channels :. _ :. _) = extent x
                        imgVecs =
                          L.map
                            (\i ->
                                VU.map (:+ 0) . toUnboxed . computeS . R.slice x $
                                (Z :. i :. All :. All))
                            [0 .. channels - 1]
                        zs =
                          L.map
                            (\pf ->
                                applyV4FilterConvolution
                                  pf
                                  downsampleFactor
                                  gFilters
                                  imgVecs)
                            pFilters
                    in (fromIntegral label', L.concat zs))
                xs
        sourceList ys
        applyV4SeparableFilterConvolutionLabeledArrayConduitP
          parallelParams
          downsampleFactor
          gFilters
          pFilters)




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
