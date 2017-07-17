{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}

module CV.Filter.PinwheelWavelet
  ( module F
  , PinwheelWaveletParams(..)
  , PinwheelWaveletExpansion
  , PinwheelWaveletConvolution
  -- , applyPinwheelWaveletExpansionGaussian
  -- , applyPinwheelWaveletConvolutionGaussian
  ) where

import           Control.Monad               as M
import           Control.Monad.IO.Class
import           CV.Filter                   as F
import           CV.Filter.GaussianFilter
import           CV.Utility.Coordinates
import           CV.Utility.FFT
import           CV.Utility.Parallel
import           CV.Utility.RepaArrayUtility
import           Data.Array.Repa             as R
import           Data.Complex                as C
import           Data.List                   as L
import           Data.Vector.Storable        as VS
import           Data.Vector.Unboxed         as VU


data PinwheelWaveletParams = PinwheelWaveletParams
  { pinwheelWaveletRows          :: !Int
  , pinwheelWaveletCols          :: !Int
  , pinwheelWaveletGaussianScale :: !Double
  , pinwheelWaveletScale         :: ![Double]
  , pinwheelWaveletRadialScale   :: ![Double]
  , pinwheelWaveletRadialFreqs   :: !Double
  , pinwheelWaveletAngularFreqs  :: ![Int]
  , pinwheelWaveletRadius        :: ![Double]
  } deriving (Show, Read)

type PinwheelWaveletExpansion = Filter PinwheelWaveletParams [[[[VU.Vector (Complex Double)]]]]
type PinwheelWaveletConvolution = Filter PinwheelWaveletParams [[[[VS.Vector (Complex Double)]]]]

instance FilterExpansion PinwheelWaveletExpansion where
  type FilterExpansionParameters PinwheelWaveletExpansion = PinwheelWaveletParams
  {-# INLINE makeFilterExpansion #-}
  makeFilterExpansion params@(PinwheelWaveletParams rows cols gScale waveletScales radialScales rf afs radiuses) rCenter cCenter =
    Filter params $!
    [ [ [ [ VU.fromListN (rows * cols) $
           makeFilterExpansionList
             rows
             cols
             rCenter
             cCenter
             (pinwheelWavelet gScale radialScale waveletScale rf af radius)
          | radius <- radiuses ]
        | af <- afs ]
      | waveletScale <- waveletScales ]
    | radialScale <- radialScales ]
  {-# INLINE getFilterExpansionNum #-}
  getFilterExpansionNum (Filter (PinwheelWaveletParams _ _ _ scales radialScales _ afs radiuses) _) =
    L.length scales * L.length afs * L.length radiuses * L.length radialScales
  {-# INLINE applyFilterExpansion #-}
  applyFilterExpansion (Filter _ filters) =
    L.concatMap
      (\x ->
          L.concatMap
            (L.concatMap (L.concatMap (L.map (VU.sum . VU.zipWith (*) x))))
            filters)
  {-# INLINE getFilterExpansionList #-}
  getFilterExpansionList = L.concatMap (L.concatMap L.concat) . getFilter

instance FilterConvolution PinwheelWaveletConvolution where
  type FilterConvolutionParameters PinwheelWaveletConvolution = PinwheelWaveletParams
  {-# INLINE makeFilterConvolution #-}
  makeFilterConvolution fftw params@(PinwheelWaveletParams rows cols gScale waveletScales radialScales rf afs radiuses) filterType =
    Filter params <$!>
    M.mapM
      (\radialScale ->
          M.mapM
            (\waveletScale ->
                M.mapM
                  (\af ->
                      M.mapM
                        (\radius ->
                            dft2d fftw rows cols .
                            VS.fromListN (rows * cols) . conjugateFunc filterType $!
                            makeFilterConvolutionList
                              rows
                              cols
                              (pinwheelWavelet
                                 gScale
                                 radialScale
                                 waveletScale
                                 rf
                                 af
                                 radius))
                        radiuses)
                  afs)
            waveletScales)
      radialScales
  {-# INLINE getFilterConvolutionNum #-}
  getFilterConvolutionNum (Filter (PinwheelWaveletParams _ _ _ scales radialScales _ afs radiuses) _) =
    L.length scales * L.length afs * L.length radiuses * L.length radialScales
  {-# INLINE applyFilterConvolution #-}
  applyFilterConvolution fftw (Filter (PinwheelWaveletParams rows cols _ _ _ _ _ _) filters) xs = do
    ys <- M.mapM (dft2d fftw rows cols) xs
    L.concat <$>
      M.mapM
        (\x ->
            L.concat <$>
            M.mapM
              (fmap L.concat .
               M.mapM
                 (fmap L.concat .
                  M.mapM (M.mapM (idft2d fftw rows cols . VS.zipWith (*) x))))
              filters)
        ys
  {-# INLINE getFilterConvolutionList #-}
  getFilterConvolutionList = L.concatMap (L.concatMap L.concat) . getFilter


{-# INLINE pinwheelWavelet #-}

pinwheelWavelet :: Double -> Double -> Double -> Double -> Int -> Double -> Int -> Int -> Complex Double
pinwheelWavelet gaussianScale radialScale waveletScale rf af shift x y
  | x == 0 && y == 0 = 0
  | otherwise =
    (radialScale / sqrt waveletScale :+ 0) *
    (gaussian1D gaussianScale (r - r') :+ 0) *
    exp
      (0 :+ fromIntegral af * angleFunctionRad (fromIntegral x) (fromIntegral y)) *
    radialFunc rf r r'
  where
    r =
      (sqrt . fromIntegral $ x ^ (2 :: Int) + y ^ (2 :: Int)) * radialScale / waveletScale
    r' = shift / waveletScale

{-# INLINE radialFunc #-}

radialFunc :: Double -> Double -> (Double -> Complex Double)
radialFunc rFreq r r' = exp (0 :+ rFreq * (r - r'))

-- applyPinwheelWaveletExpansionGaussian
--   :: FFTW
--   -> PinwheelWaveletExpansion
--   -> GaussianFilterConvolution1D
--   -> [VU.Vector (Complex Double)]
--   -> IO [Complex Double]
-- applyPinwheelWaveletExpansionGaussian fftw (Filter _ pFilters) gFilter imgVecs = do
--   let xs =
--         L.concatMap
--           (\imgVec ->
--               L.concat .
--               parMap
--                 rdeepseq
--                 (L.map
--                    (VS.fromList .
--                     L.map
--                       ((\x -> magnitude x :+ 0) . VU.sum . VU.zipWith (*) imgVec))) $
--               pFilters)
--           imgVecs
--   ys <- applyFilterConvolution fftw gFilter xs
--   return . L.concatMap VS.toList $ ys


-- applyPinwheelWaveletConvolutionGaussian
--   :: FFTW
--   -> PinwheelWaveletConvolution
--   -> GaussianFilterConvolution1D
--   -> Int
--   -> [VS.Vector (Complex Double)]
--   -> IO [VS.Vector (Complex Double)]
-- applyPinwheelWaveletConvolutionGaussian fftw (Filter (PinwheelWaveletParams rows cols _ _ _ _ _) pFilters) gFilters stride imgVecs = do
--   imgVecsF <- M.mapM (dft2d fftw rows cols) imgVecs
--   xs <-
--     M.mapM
--       (\imgVec ->
--           M.mapM
--             (M.mapM
--                (fmap
--                   (L.map (VS.fromList . L.map (\x -> magnitude x :+ 0)) .
--                    L.transpose .
--                    if stride == 1
--                      then L.map VS.toList
--                      else L.map
--                             (R.toList .
--                              downsample [stride, stride] .
--                              fromUnboxed (Z :. rows :. cols) . VS.convert)) .
--                 M.mapM (idft2d fftw rows cols . VS.zipWith (*) imgVec)))
--             pFilters)
--       imgVecsF :: IO [[[[VS.Vector (Complex Double)]]]]
--   ys <-
--     L.concat <$>
--     M.mapM
--       (fmap L.concat .
--        M.mapM
--          (fmap L.concat .
--           M.mapM
--             (fmap (L.transpose . L.map VS.toList) .
--              applyFilterConvolution fftw gFilters)))
--       xs
--   return . L.map VS.fromList . L.transpose $ ys
