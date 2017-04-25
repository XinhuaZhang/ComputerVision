{-# LANGUAGE BangPatterns #-}
module CV.V4FilterConvolution where

import           Control.Arrow
import           Control.DeepSeq
import           Control.Monad                    as M
import           Control.Monad.Trans.Resource
import           CV.Array.LabeledArray
import           CV.Filter.CartesianGratingFilter as V4
import           CV.Filter.HyperbolicFilter       as V4
import           CV.Filter.PolarSeparableFilter   as V4 hiding (makeFilter)
import           CV.FilterExpansion
import           CV.Utility.Coordinates
import           CV.Utility.Parallel
import           CV.Utility.RepaArrayUtility
import           Data.Array.CArray                as CA
import           Data.Array.Repa                  as R
import           Data.Complex
import           Data.Conduit
import           Data.Conduit.List                as CL
import           Data.List                        as L
import           Data.Ord
import           Data.Vector.Unboxed              as VU
import           Math.FFT


{-# INLINE twoDFourierTransform #-}

twoDFourierTransform :: (Int, Int)
                     -> VU.Vector (Complex Double)
                     -> VU.Vector (Complex Double)
twoDFourierTransform (rows, cols) =
  VU.fromListN (rows * cols) .
  CA.elems . dftN [0, 1] . listArray ((0, 0), (rows - 1, cols - 1)) . VU.toList



{-# INLINE fourierTransform #-}

fourierTransform :: (Int, Int) -> V4SeparableFilter -> V4SeparableFilterConvolution
fourierTransform (rows, cols) (V4PolarSeparableFilterAxis freqs vecs) =
  V4PolarSeparableFilterConvolutionAxis (rows, cols) freqs .
  L.map (L.map (twoDFourierTransform (rows, cols))) $
  vecs
fourierTransform _ _ = error "fourierTransform: filter type is not supported."

{-# INLINE applyFilterConvolution #-}

applyFilterConvolution
  :: (Int, Int)
  -> VU.Vector (Complex Double)
  -> [VU.Vector (Complex Double)]
  -> [VU.Vector (Complex Double)]
applyFilterConvolution (rows, cols) imgVec =
  L.map
    (VU.fromListN (rows * cols) .
     CA.elems .
     idftN [0, 1] .
     listArray ((0, 0), (rows - 1, cols - 1)) . VU.toList . VU.zipWith (*) imgVec)

{-# INLINE applyV4SeparableFilterConvolution #-}

applyV4SeparableFilterConvolution :: V4SeparableFilterConvolution
                                  -> VU.Vector (Complex Double)
                                  -> VU.Vector Double
applyV4SeparableFilterConvolution (V4PolarSeparableFilterConvolutionAxis (rows, cols) freqs filters) imgVec =
  VU.fromList .
  L.concatMap
    (\xs ->
       let filteredImg = applyFilterConvolution (rows, cols) imgVec xs
           magnitudeImg = L.map (VU.map magnitude) filteredImg
           -- idx = findGlobalMaximaSum magnitudeImg
           idx = ((div rows 2) - 1) * cols + div cols 2
           mag = L.map (VU.! idx) magnitudeImg
           phaseDiff =
             computePhaseDifference $ L.zip3 freqs filteredImg magnitudeImg
       in -- mag -- L.++
           phaseDiff
     ) $
  filters
  -- where
  --   imgVecF = twoDFourierTransform (rows, cols) imgVec


{-# INLINE findGlobalMaximaMultiply #-}

findGlobalMaximaMultiply :: VU.Vector Double -> VU.Vector Double -> Int
findGlobalMaximaMultiply vec1 vec2 = VU.maxIndex $ VU.zipWith (*) vec1 vec2

{-# INLINE findGlobalMaximaSum #-}

findGlobalMaximaSum :: [VU.Vector Double] -> Int
findGlobalMaximaSum = VU.maxIndex . L.foldl1' (VU.zipWith (+))

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
    in [mag, y, z]
  | otherwise =
    let (!y :+ (!z)) = (xn ** (mn / abs fn :+ 0)) * (xm ** (mn / abs fm :+ 0))
    in [mag, y, z]
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
        if fm == 0
          then []
          else phaseDifference (findGlobalMaximaMultiply xnM xmM) xnC fn xmC fm)
    xs L.++
  computePhaseDifference xs



applyV4SeparableFilterConvolutionLabeledArrayConduit
  :: ParallelParams
  -> [V4SeparableFilterConvolution]
  -> Conduit (LabeledArray DIM3 Double) (ResourceT IO) (Double,VU.Vector Double)
applyV4SeparableFilterConvolutionLabeledArrayConduit parallelParams !filters = do
  xs <- CL.take (batchSize parallelParams)
  unless
    (L.null xs)
    (do let ys =
              L.map
                (\(LabeledArray label' x) ->
                   let (Z :. channels :. rows :. cols) = extent x
                       imgVecs =
                         L.map
                           (\i ->
                              twoDFourierTransform (rows, cols) .
                              VU.map (:+ 0) . toUnboxed . computeS . R.slice x $
                              (Z :. i :. All :. All))
                           [0 .. channels - 1]
                   in LabeledArray label' .
                      R.fromUnboxed (Z :. channels :. rows :. cols) . VU.concat $
                      imgVecs)
                xs
            zs =
              parMapChunk
                parallelParams
                rdeepseq
                (\(LabeledArray label' x) ->
                   let (Z :. channels :. _ :. _) = extent x
                       imgVecs =
                         L.map
                           (\i ->
                              toUnboxed . computeS . R.slice x $
                              (Z :. i :. All :. All))
                           [0 .. channels - 1]
                   in ( fromIntegral label'
                        -- normalizeVec .
                      , VU.concat .
                        L.concatMap
                          (\img ->
                             L.map
                               (\filter' ->
                                  applyV4SeparableFilterConvolution filter' img)
                               filters) $
                        imgVecs)) $!!
              ys
        sourceList zs
        applyV4SeparableFilterConvolutionLabeledArrayConduit
          parallelParams
          filters)

{-# INLINE normalizeVec #-}

normalizeVec :: VU.Vector Double -> VU.Vector Double
normalizeVec vec
  | s == 0 = VU.replicate (VU.length vec) 0
  | otherwise = VU.map (/ s) vec
  where
    s = sqrt . VU.sum . VU.map (^ 2) $ vec
