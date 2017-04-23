{-# LANGUAGE BangPatterns #-}
module CV.V4FilterConvolution where

import           Control.Arrow
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

{-# INLINE fourierTransform #-}

fourierTransform :: (Int, Int) -> V4SeparableFilter -> V4SeparableFilterConvolution
fourierTransform (rows, cols) (V4PolarSeparableFilterAxis freqs vecs) =
  V4PolarSeparableFilterConvolutionAxis (rows, cols) freqs .
  L.map
    (L.map
       (VU.fromListN (rows * cols) .
        CA.elems . dftN [0, 1] . listArray ((0, 0), (rows - 1, cols - 1)) . VU.toList)) $
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
        let filteredImg = applyFilterConvolution (rows, cols) imgVecF xs
            magnitudeImg = L.map (VU.map magnitude) filteredImg
            idx = findGlobalMaximaSum magnitudeImg
            mag = L.map (VU.! idx) magnitudeImg
            phaseDiff = computePhaseDifference $ L.zip3 freqs filteredImg magnitudeImg
        in mag L.++ phaseDiff) $
  filters
  where
    imgVecF =
      VU.fromListN (rows * cols) .
      CA.elems . dftN [0, 1] . listArray ((0, 0), (rows - 1, cols - 1)) . VU.toList $
      imgVec


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
    in [y, z]
  | otherwise =
    let (!y :+ (!z)) = (xn ** (mn / abs fn :+ 0)) * (xm ** (mn / abs fm :+ 0))
    in [y, z]
  where
    xn = (vec1 VU.! idx) / (magnitude (vec1 VU.! idx) :+ 0)
    xm = (vec2 VU.! idx) / (magnitude (vec2 VU.! idx) :+ 0)
    mn = fromIntegral $ lcm (round . abs $ fm) (round . abs $ fn) :: Double


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
applyV4SeparableFilterConvolutionLabeledArrayConduit parallelParams filters = do
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
                    in ( fromIntegral label'
                       , VU.concat .
                         L.concatMap
                           (\img ->
                               L.map
                                 (\filter' ->
                                     applyV4SeparableFilterConvolution filter' img)
                                 filters) $
                         imgVecs))
                xs
        sourceList ys
        applyV4SeparableFilterConvolutionLabeledArrayConduit parallelParams filters)
