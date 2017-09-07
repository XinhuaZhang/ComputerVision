{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}

module CV.Filter.PinwheelWavelet
  ( module F
  , PinwheelWaveletParams(..)
  , PinwheelWaveletExpansion
  , PinwheelWaveletConvolution
  , PinwheelWaveletPIConvolution
  , PinwheelWaveletPIExpansion(..)
  , applyPinwheelWaveletConvolution
  , applyPinwheelWaveletExpansion
  ) where

import           Control.Monad            as M
import           CV.Filter                as F
import           CV.Filter.GaussianFilter
import           CV.Utility.Coordinates
import           CV.Utility.FFT
import           CV.Utility.Parallel
import           Data.Complex             as C
import           Data.List                as L
import           Data.Vector.Storable     as VS
import           Data.Vector.Unboxed      as VU


data PinwheelWaveletParams = PinwheelWaveletParams
  { pinwheelWaveletRows          :: !Int
  , pinwheelWaveletCols          :: !Int
  , pinwheelWaveletGaussianScale :: !Double
  , pinwheelWaveletScale         :: ![Double]
  , pinwheelWaveletRadialScale   :: ![Double]
  , pinwheelWaveletRadialFreqs   :: ![Double]
  , pinwheelWaveletAngularFreqs  :: ![Int]
  , pinwheelWaveletRadius        :: ![Double]
  } deriving (Show, Read)

type PinwheelWaveletExpansion = Filter PinwheelWaveletParams [[[[VU.Vector (Complex Double)]]]]
type PinwheelWaveletConvolution = Filter PinwheelWaveletParams [[[[VS.Vector (Complex Double)]]]]
data PinwheelWaveletPIConvolution = PinwheelWaveletPIConvolution (Filter PinwheelWaveletParams [[[[VS.Vector (Complex Double)]]]])
data PinwheelWaveletPIExpansion = PinwheelWaveletPIExpansion (Filter PinwheelWaveletParams [[[[VU.Vector (Complex Double)]]]])

instance FilterExpansion PinwheelWaveletExpansion where
  type FilterExpansionParameters PinwheelWaveletExpansion = PinwheelWaveletParams
  {-# INLINE makeFilterExpansion #-}
  makeFilterExpansion params@(PinwheelWaveletParams rows cols gScale waveletScales radialScales rfs afs radiuses) rCenter cCenter =
    Filter params $!
    [ [ [ [ VU.fromListN (rows * cols) $
           makeFilterExpansionList
             rows
             cols
             rCenter
             cCenter
             (pinwheelWavelet gScale radialScale waveletScale rf af radius)
          | radius <- radiuses ]
        | af <- afs
        , rf <- rfs ]
      | waveletScale <- waveletScales ]
    | radialScale <- radialScales ]
  {-# INLINE getFilterExpansionNum #-}
  getFilterExpansionNum (Filter (PinwheelWaveletParams _ _ _ scales radialScales rfs afs radiuses) _) =
    L.length scales * L.length afs * L.length radiuses * L.length radialScales *
    L.length rfs
  {-# INLINE applyFilterExpansion #-}
  applyFilterExpansion (Filter _ filters) =
    L.concatMap
      (\x ->
          L.concatMap
            (L.concatMap (L.concatMap (L.map (VU.sum . VU.zipWith (*) x))))
            filters)
  {-# INLINE getFilterExpansionList #-}
  getFilterExpansionList = L.concatMap (L.concatMap L.concat) . getFilter
  
instance FilterExpansion PinwheelWaveletPIExpansion where
  type FilterExpansionParameters PinwheelWaveletPIExpansion = PinwheelWaveletParams
  {-# INLINE makeFilterExpansion #-}
  makeFilterExpansion params@(PinwheelWaveletParams rows cols gScale waveletScales radialScales rfs afs radiuses) rCenter cCenter =
    PinwheelWaveletPIExpansion . Filter params $!
    [ [ [ [ VU.fromListN (rows * cols) $
           makeFilterExpansionList
             rows
             cols
             rCenter
             cCenter
             (pinwheelWaveletPI gScale radialScale waveletScale rf af radius)
          | radius <- radiuses ]
        | af <- afs
        , rf <- rfs ]
      | waveletScale <- waveletScales ]
    | radialScale <- radialScales ]
  {-# INLINE getFilterExpansionNum #-}
  getFilterExpansionNum (PinwheelWaveletPIExpansion (Filter (PinwheelWaveletParams _ _ _ scales radialScales rfs afs radiuses) _)) =
    L.length scales * L.length afs * L.length radiuses * L.length radialScales *
    L.length rfs
  {-# INLINE applyFilterExpansion #-}
  applyFilterExpansion (PinwheelWaveletPIExpansion (Filter _ filters)) =
    L.concatMap
      (\x ->
          L.concatMap
            (L.concatMap (L.concatMap (L.map (VU.sum . VU.zipWith (*) x))))
            filters)
  {-# INLINE getFilterExpansionList #-}
  getFilterExpansionList =
    L.concatMap (L.concatMap L.concat) .
    getFilter . (\(PinwheelWaveletPIExpansion x) -> x)

instance FilterConvolution PinwheelWaveletConvolution where
  type FilterConvolutionParameters PinwheelWaveletConvolution = PinwheelWaveletParams
  {-# INLINE makeFilterConvolution #-}
  makeFilterConvolution fftw params@(PinwheelWaveletParams rows cols gScale waveletScales radialScales rfs afs radiuses) filterType =
    Filter params <$!>
    M.mapM
      (\radialScale ->
          M.mapM
            (\waveletScale ->
                M.mapM
                  (\radius ->
                      M.mapM
                        (\(af,rf) ->
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
                        $ [(af,rf) | af <- afs, rf <- rfs])
                  radiuses)
            waveletScales)
      radialScales
  {-# INLINE getFilterConvolutionNum #-}
  getFilterConvolutionNum (Filter (PinwheelWaveletParams _ _ _ scales radialScales rfs afs radiuses) _) =
    L.length scales * L.length afs * L.length radiuses * L.length radialScales * L.length rfs
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

pinwheelWavelet :: Double
                -> Double
                -> Double
                -> Double
                -> Int
                -> Double
                -> Int
                -> Int
                -> Complex Double
pinwheelWavelet gaussianScale radialScale waveletScale rf af shift x y
  | x == 0 && y == 0 && shift == 0 = 
    (radialScale ** (3 / 2) :+ 0) / (2 * pi * sqrt waveletScale :+ 0) * (gaussian1D gaussianScale 0 :+ 0) 
  | x == 0 && y == 0 && shift /= 0 = 0
  | otherwise =
    (radialScale ** (3 / 2) :+ 0) / (2 * pi * sqrt waveletScale :+ 0) *
    (gaussian1D gaussianScale (r - r') :+ 0) *
    exp
      (0 :+ fromIntegral af * angleFunctionRad (fromIntegral x) (fromIntegral y)) *
    radialFunc rf r r'
  where
    r =
      (log . sqrt . fromIntegral $ x ^ (2 :: Int) + y ^ (2 :: Int)) * radialScale / waveletScale
    r' = shift --  / waveletScale
    

instance FilterConvolution PinwheelWaveletPIConvolution where
  type FilterConvolutionParameters PinwheelWaveletPIConvolution = PinwheelWaveletParams
  {-# INLINE makeFilterConvolution #-}
  makeFilterConvolution fftw params@(PinwheelWaveletParams rows cols gScale waveletScales radialScales rfs afs radiuses) filterType =
    PinwheelWaveletPIConvolution . Filter params <$!>
    M.mapM
      (\radialScale ->
          M.mapM
            (\waveletScale ->
                M.mapM
                  (\radius ->
                      M.mapM
                        (\(af, rf) ->
                            dft2d fftw rows cols . VS.fromListN (rows * cols) .
                            conjugateFunc filterType $!
                            makeFilterConvolutionList
                              rows
                              cols
                              (pinwheelWaveletPI
                                 gScale
                                 radialScale
                                 waveletScale
                                 rf
                                 af
                                 radius)) $
                      [ (af, rf)
                      | af <- afs
                      , rf <- rfs ])
                  radiuses)
            waveletScales)
      radialScales
  {-# INLINE getFilterConvolutionNum #-}
  getFilterConvolutionNum (PinwheelWaveletPIConvolution (Filter (PinwheelWaveletParams _ _ _ scales radialScales rfs afs radiuses) _)) =
    L.length scales * L.length afs * L.length radiuses * L.length radialScales *
    L.length rfs
  {-# INLINE applyFilterConvolution #-}
  applyFilterConvolution fftw (PinwheelWaveletPIConvolution (Filter (PinwheelWaveletParams rows cols _ _ _ _ _ _) filters)) xs = do
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
  getFilterConvolutionList =
    L.concatMap (L.concatMap L.concat) . getFilter .
    (\(PinwheelWaveletPIConvolution x) -> x)

{-# INLINE pinwheelWaveletPI #-}

pinwheelWaveletPI :: Double
                  -> Double
                  -> Double
                  -> Double
                  -> Int
                  -> Double
                  -> Int
                  -> Int
                  -> Complex Double
pinwheelWaveletPI gaussianScale radialScale waveletScale rf af shift x y
  | x == 0 && y == 0 && shift == 0 = 
    (radialScale ** (3 / 2) :+ 0) / (2 * pi * sqrt waveletScale :+ 0) * (gaussian1D gaussianScale 0 :+ 0) 
  | x == 0 && y == 0 && shift /= 0 = 0
  | otherwise =
    (radialScale ** (3 / 2) :+ 0) / (2 * pi * sqrt waveletScale :+ 0) *
    (gaussian1D gaussianScale (r - r') :+ 0) *
    exp
      (0 :+
       fromIntegral af *
       angleFunctionRad (fromIntegral (-x)) (fromIntegral (-y))) *
    radialFunc rf r r'
  where
    r =
      (log . sqrt . fromIntegral $ x ^ (2 :: Int) + y ^ (2 :: Int)) * radialScale / waveletScale
    r' = shift --  / waveletScale

{-# INLINE radialFunc #-}

radialFunc :: Double  -> Double -> (Double -> Complex Double)
radialFunc rFreq r r' = exp (0 :+ rFreq * ( r - r'))

{-# INLINE applyPinwheelWaveletExpansion #-}

-- applyPinwheelWaveletExpansion
--   :: PinwheelWaveletExpansion
--   -> [VU.Vector (Complex Double)]
--   -> [[[[Complex Double]]]]
-- applyPinwheelWaveletExpansion (Filter _ pFilters) imgVecs =
--   L.map
--     (\scales ->
--         L.concatMap
--           (\imgVec ->
--               parMap rdeepseq (L.map (L.map (VU.sum . VU.zipWith (*) imgVec))) scales)
--           imgVecs)
--     pFilters
    
applyPinwheelWaveletExpansion
  :: PinwheelWaveletExpansion
  -> [VU.Vector (Complex Double)]
  -> [[[Complex Double]]]
applyPinwheelWaveletExpansion (Filter _ pFilters) imgVecs =
  L.map
    (\scales ->
        L.concatMap
          (\imgVec ->
              parMap rdeepseq (L.map (VU.sum . VU.zipWith (*) imgVec)) . L.concat $
              scales)
          imgVecs)
    pFilters

{-# INLINE applyPinwheelWaveletConvolution #-}

applyPinwheelWaveletConvolution
  :: FFTW
  -> PinwheelWaveletConvolution
  -> [VS.Vector (Complex Double)]
  -> IO [[[VS.Vector (Complex Double)]]]
applyPinwheelWaveletConvolution fftw (Filter (PinwheelWaveletParams rows cols _ _ _ _ _ _) pFilters) imgVecs = do
  imgVecsF <- M.mapM (dft2d fftw rows cols) imgVecs
  M.mapM
    (\filters ->
        L.concat <$>
        M.mapM
          (\imgVec ->
              L.concat <$>
              M.mapM
                (M.mapM (M.mapM (idft2d fftw rows cols . VS.zipWith (*) imgVec)))
                filters)
          imgVecsF)
    pFilters

