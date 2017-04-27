{-# LANGUAGE BangPatterns #-}
module CV.V4FilterConvolution where

import           Control.Arrow
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
import           Data.Array.CArray                as CA
import           Data.Array.Repa                  as R
import           Data.Complex
import           Data.Conduit
import           Data.Conduit.List                as CL
import           Data.List                        as L
import           Data.Vector.Unboxed              as VU


{-# INLINE fourierTransformFilter #-}

fourierTransformFilter :: (Int, Int) -> V4SeparableFilter -> IO V4SeparableFilterConvolution
fourierTransformFilter (rows, cols) (V4PolarSeparableFilterAxis freqs vecs) =
  fmap (V4PolarSeparableFilterConvolutionAxis (rows, cols) freqs) .
  M.mapM
    (M.mapM (dftN [0, 1] . listArray ((0, 0), (rows - 1, cols - 1)) . VU.toList)) $
  vecs
fourierTransformFilter _ _ =
  error "fourierTransform: filter type is not supported."

{-# INLINE applyFilterConvolution #-}

applyFilterConvolution
  :: (Int, Int)
  -> VU.Vector (Complex Double)
  -> [CArray (Int, Int) (Complex Double)]
  -> IO [VU.Vector (Complex Double)]
applyFilterConvolution (rows, cols) imgVec filters' = do
  imgVecF <-
    dftN [0, 1] . listArray ((0, 0), (rows - 1, cols - 1)) . VU.toList $ imgVec
  let xs = L.map (liftArray2 (*) imgVecF) filters'
  M.mapM (fmap (VU.fromListN (rows * cols) . CA.elems) . idftN [0, 1]) xs

{-# INLINE applyV4FilterConvolution #-}

applyV4FilterConvolution
  :: V4SeparableFilterConvolution
  -> [VU.Vector (Complex Double)]
  -> IO [V4SeparableFilteredImageConvolution]
applyV4FilterConvolution (V4PolarSeparableFilterConvolutionAxis (rows, cols) freqs filters) =
  M.mapM
    (\imgVec ->
        fmap (V4PolarSeparableFilteredImageConvolutionAxis (rows, cols) freqs) .
        M.mapM (applyFilterConvolution (rows, cols) imgVec) $
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
            idx = ((div rows 2) - 1) * cols + div cols 2
            mag = L.map (VU.! idx) magnitudeImg
            phaseDiff = computePhaseDifference $ L.zip3 freqs filteredImg magnitudeImg
            -- mag -- L.++
        in phaseDiff) $
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
findGlobalMaximaCenterOfMass (rows, cols) vec1 vec2 =
  join (***) (\x -> round $ x / VU.sum vec) .
  L.foldl' (\(s1, s2) ((i, j), v) -> (s1 + i * v, s2 + j * v)) (0, 0) .
  L.zip [(fromIntegral i, fromIntegral j) | i <- [1 .. rows], j <- [1 .. cols]] .
  VU.toList $
  VU.zipWith (*) vec1 vec2
  where
    vec = VU.zipWith (*) vec1 vec2

{-# INLINE phaseDifference #-}

phaseDifference
  :: Int
  -> VU.Vector (Complex Double)
  -> Double
  -> VU.Vector (Complex Double)
  -> Double
  -> [Double]
phaseDifference idx vec1 fn vec2 fm
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
    mag =  min (mag1 / mag2) (mag2 / mag1)


{-# INLINE computePhaseDifference #-}

computePhaseDifference :: [(Double, VU.Vector (Complex Double), VU.Vector Double)]
                       -> [Double]
computePhaseDifference [] = []
computePhaseDifference ((0, _, _):xs) = computePhaseDifference xs
computePhaseDifference ((fn, xnC, xnM):xs) =
  L.concatMap
    (\(fm, xmC, xmM) ->
        let -- idxs = findGlobalMaximaMultiply1 xnM xmM
            --idxs = [63 * 128 + 64]
            rows = 128
            cols = 128
            (a,b) = findGlobalMaximaCenterOfMass (rows,cols) xnM xmM
            idxs = [a*cols + b]
        in if fm == 0
             then []
             else L.foldl1' (L.zipWith (+)) .
                  L.map (\idx' -> phaseDifference idx' xnC fn xmC fm) $
                  idxs)
    xs L.++
  computePhaseDifference xs

applyV4SeparableFilterConvolutionLabeledArrayConduit
  :: [V4SeparableFilterConvolution]
  -> Conduit (LabeledArray DIM3 Double) (ResourceT IO) (Double, [V4SeparableFilteredImageConvolution])
applyV4SeparableFilterConvolutionLabeledArrayConduit filters =
  awaitForever
    (\(LabeledArray label' x) -> do
       let (Z :. channels :. _ :. _) = extent x
           imgVecs =
             L.map
               (\i ->
                   VU.map (:+ 0) . toUnboxed . computeS . R.slice x $
                   (Z :. i :. All :. All))
               [0 .. channels - 1]
       ys <- liftIO $ M.mapM (`applyV4FilterConvolution` imgVecs) filters
       yield (fromIntegral label', L.concat ys))



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
                (VU.concat . L.map calculateV4SeparableFilterConvolutionFeature)
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
