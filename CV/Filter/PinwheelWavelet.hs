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
  -- , applyPinwheelWaveletExpansion
  ) where

import           Control.Monad            as M
import           CV.Filter                as F
import           CV.Filter.GaussianFilter
import           CV.Utility.Coordinates
import           CV.Utility.DFT
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
  , pinwheelWaveletRadialFreqs   :: ![Double]
  , pinwheelWaveletAngularFreqs  :: ![Int]
  , pinwheelWaveletRadius        :: ![Double]
  } deriving (Show, Read)

type PinwheelWaveletExpansion = Filter PinwheelWaveletParams [[[VU.Vector (Complex Double)]]]
type PinwheelWaveletConvolution = Filter PinwheelWaveletParams [[[VS.Vector (Complex Double)]]]
data PinwheelWaveletPIConvolution = PinwheelWaveletPIConvolution (Filter PinwheelWaveletParams [[[[VS.Vector (Complex Double)]]]])
data PinwheelWaveletPIExpansion = PinwheelWaveletPIExpansion (Filter PinwheelWaveletParams [[[[VU.Vector (Complex Double)]]]])

instance FilterExpansion PinwheelWaveletExpansion where
  type FilterExpansionParameters PinwheelWaveletExpansion = PinwheelWaveletParams
  {-# INLINE makeFilterExpansion #-}
  makeFilterExpansion params@(PinwheelWaveletParams rows cols gScale waveletScales rfs afs radiuses) rCenter cCenter =
    Filter params $!
    [ [ [ VU.fromListN (rows * cols) $
    makeFilterExpansionList
      rows
      cols
      rCenter
      cCenter
      (pinwheelWavelet gScale waveletScale rf af radius)
    | af <- afs
    , rf <- rfs
    ]
    | waveletScale <- waveletScales
    ]
    | radius <- radiuses
    ]
  {-# INLINE getFilterExpansionNum #-}
  getFilterExpansionNum (Filter (PinwheelWaveletParams _ _ _ scales rfs afs radiuses) _) =
    L.length scales * L.length afs * L.length radiuses * L.length rfs
  {-# INLINE applyFilterExpansion #-}
  applyFilterExpansion (Filter _ filters) =
    L.concatMap
      (\x ->
         L.concatMap (L.concatMap (L.map (VU.sum . VU.zipWith (*) x))) filters)
  {-# INLINE getFilterExpansionList #-}
  getFilterExpansionList = L.concatMap L.concat . getFilter
  
-- instance FilterExpansion PinwheelWaveletPIExpansion where
--   type FilterExpansionParameters PinwheelWaveletPIExpansion = PinwheelWaveletParams
--   {-# INLINE makeFilterExpansion #-}
--   makeFilterExpansion params@(PinwheelWaveletParams rows cols gScale waveletScales radialScales rfs afs radiuses) rCenter cCenter =
--     PinwheelWaveletPIExpansion . Filter params $!
--     [ [ [ [ VU.fromListN (rows * cols) $
--     makeFilterExpansionList
--       rows
--       cols
--       rCenter
--       cCenter
--       (pinwheelWaveletPI gScale radialScale waveletScale rf af radius)
--     | af <- afs
--     , rf <- rfs
--     ]
--     | radius <- radiuses
--     ]
--     | waveletScale <- waveletScales
--     ]
--     | radialScale <- radialScales
--     ]
--   {-# INLINE getFilterExpansionNum #-}
--   getFilterExpansionNum (PinwheelWaveletPIExpansion (Filter (PinwheelWaveletParams _ _ _ scales radialScales rfs afs radiuses) _)) =
--     L.length scales * L.length afs * L.length radiuses * L.length radialScales *
--     L.length rfs
--   {-# INLINE applyFilterExpansion #-}
--   applyFilterExpansion (PinwheelWaveletPIExpansion (Filter _ filters)) =
--     L.concatMap
--       (\x ->
--          L.concatMap
--            (L.concatMap (L.concatMap (L.map (VU.sum . VU.zipWith (*) x))))
--            filters)
--   {-# INLINE getFilterExpansionList #-}
--   getFilterExpansionList =
--     L.concatMap (L.concatMap L.concat) .
--     getFilter . (\(PinwheelWaveletPIExpansion x) -> x)

instance FilterConvolution PinwheelWaveletConvolution where
  type FilterConvolutionParameters PinwheelWaveletConvolution = PinwheelWaveletParams
  {-# INLINE makeFilterConvolution #-}
  makeFilterConvolution plan params@(PinwheelWaveletParams rows cols gScale waveletScales rfs afs radiuses) filterType = do
    let filterList =
          L.map
            (\radius ->
               L.map
                 (\waveletScale ->
                    L.map
                      (\(af, rf) ->
                         VS.fromList . conjugateFunc filterType $!
                         makeFilterConvolutionList
                           rows
                           cols
                           (pinwheelWavelet gScale waveletScale rf af radius)) $
                    [(af, rf) | af <- afs, rf <- rfs])
                 waveletScales)
            radiuses
        filterTmp =
          VS.fromListN (rows * cols) . conjugateFunc filterType $!
          makeFilterConvolutionList
            rows
            cols
            (pinwheelWavelet
               gScale
               (L.last waveletScales)
               (L.last rfs)
               (L.last afs)
               (L.last radiuses))
    lock <- getFFTWLock
    (p1, vec) <- dft2dPlan lock plan rows cols filterTmp
    (p2, _) <- idft2dPlan lock p1 rows cols vec
    filters <-
      Filter params <$!>
      M.mapM
        (M.mapM (dftExecuteBatch p2 (DFTPlanID DFT2D [rows, cols] [])))
        filterList
    return (p2, filters)
  {-# INLINE getFilterConvolutionNum #-}
  getFilterConvolutionNum (Filter (PinwheelWaveletParams _ _ _ scales rfs afs radiuses) _) =
    L.length scales * L.length afs * L.length radiuses * L.length rfs
  {-# INLINE applyFilterConvolution #-}
  applyFilterConvolution plan (Filter (PinwheelWaveletParams rows cols _ _ _ _ _) filters) xs = do
    ys <- dftExecuteBatch plan (DFTPlanID DFT2D [rows, cols] []) xs
    dftExecuteBatch plan (DFTPlanID IDFT2D [rows, cols] []) .
      L.concatMap
        (\x -> L.concatMap (L.concatMap (L.map (VS.zipWith (*) x))) filters) $
      ys
  {-# INLINE getFilterConvolutionList #-}
  getFilterConvolutionList = L.concatMap L.concat . getFilter


{-# INLINE pinwheelWavelet #-}

pinwheelWavelet :: Double
                -> Double
                -> Double
                -> Int
                -> Double
                -> Int
                -> Int
                -> Complex Double
pinwheelWavelet gaussianScale waveletScale rf af shift x y
  | x == 0 && y == 0 && shift == 0 =
    1 / (2 * pi * sqrt waveletScale :+ 0) * (gaussian1D gaussianScale 0 :+ 0)
  | x == 0 && y == 0 && shift /= 0 = 0
  | otherwise =
    1 / (2 * pi * sqrt waveletScale :+ 0) *
    (gaussian1D gaussianScale (r - r') :+ 0) *
    exp
      (0 :+ fromIntegral af * angleFunctionRad (fromIntegral x) (fromIntegral y)) *
    radialFunc rf r r'
  where
    r =
      (log . sqrt . fromIntegral $ x ^ (2 :: Int) + y ^ (2 :: Int)) /
      waveletScale
    r' = (log shift)  / waveletScale


-- instance FilterConvolution PinwheelWaveletPIConvolution where
--   type FilterConvolutionParameters PinwheelWaveletPIConvolution = PinwheelWaveletParams
--   {-# INLINE makeFilterConvolution #-}
--   makeFilterConvolution fftw params@(PinwheelWaveletParams rows cols gScale waveletScales radialScales rfs afs radiuses) filterType =
--     PinwheelWaveletPIConvolution . Filter params <$!>
--     M.mapM
--       (\radialScale ->
--           M.mapM
--             (\waveletScale ->
--                 M.mapM
--                   (\radius ->
--                       M.mapM
--                         (\(af, rf) ->
--                             dft2d fftw rows cols . VS.fromListN (rows * cols) .
--                             conjugateFunc filterType $!
--                             makeFilterConvolutionList
--                               rows
--                               cols
--                               (pinwheelWaveletPI
--                                  gScale
--                                  radialScale
--                                  waveletScale
--                                  rf
--                                  af
--                                  radius)) $
--                       [ (af, rf)
--                       | af <- afs
--                       , rf <- rfs ])
--                   radiuses)
--             waveletScales)
--       radialScales
--   {-# INLINE getFilterConvolutionNum #-}
--   getFilterConvolutionNum (PinwheelWaveletPIConvolution (Filter (PinwheelWaveletParams _ _ _ scales radialScales rfs afs radiuses) _)) =
--     L.length scales * L.length afs * L.length radiuses * L.length radialScales *
--     L.length rfs
--   {-# INLINE applyFilterConvolution #-}
--   applyFilterConvolution fftw (PinwheelWaveletPIConvolution (Filter (PinwheelWaveletParams rows cols _ _ _ _ _ _) filters)) xs = do
--     ys <- M.mapM (dft2d fftw rows cols) xs
--     L.concat <$>
--       M.mapM
--         (\x ->
--             L.concat <$>
--             M.mapM
--               (fmap L.concat .
--                M.mapM
--                  (fmap L.concat .
--                   M.mapM (M.mapM (idft2d fftw rows cols . VS.zipWith (*) x))))
--               filters)
--         ys
--   {-# INLINE getFilterConvolutionList #-}
--   getFilterConvolutionList =
--     L.concatMap (L.concatMap L.concat) . getFilter .
--     (\(PinwheelWaveletPIConvolution x) -> x)

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
    (radialScale ** (3 / 2) :+ 0) / (2 * pi * sqrt waveletScale :+ 0) *
    (gaussian1D gaussianScale 0 :+ 0)
  | x == 0 && y == 0 && shift /= 0 = 0
  | otherwise =
    (radialScale ** (3 / 2) :+ 0) / (2 * pi * sqrt waveletScale :+ 0) *
    (gaussian1D gaussianScale ((r - r') / waveletScale) :+ 0) *
    exp
      (0 :+
       fromIntegral af *
       angleFunctionRad (fromIntegral (-x)) (fromIntegral (-y))) *
    radialFunc rf r r'
  where
    r =
      (log . sqrt . fromIntegral $ x ^ (2 :: Int) + y ^ (2 :: Int)) *
      radialScale -- /
      -- waveletScale
    r' = (log shift) -- / waveletScale

{-# INLINE radialFunc #-}

radialFunc :: Double  -> Double -> (Double -> Complex Double)
radialFunc rFreq r r' = exp (0 :+ rFreq * ( r - r'))

-- {-# INLINE applyPinwheelWaveletExpansion #-}

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
    
-- applyPinwheelWaveletExpansion
--   :: PinwheelWaveletExpansion
--   -> [VU.Vector (Complex Double)]
--   -> [[[Complex Double]]]
-- applyPinwheelWaveletExpansion (Filter _ pFilters) imgVecs =
--   L.map
--     (\scales ->
--         L.concatMap
--           (\imgVec ->
--               parMap rdeepseq (L.map (VU.sum . VU.zipWith (*) imgVec)) . L.concat $
--               scales)
--           imgVecs)
--     pFilters

{-# INLINE applyPinwheelWaveletConvolution #-}

applyPinwheelWaveletConvolution
  :: DFTPlan
  -> PinwheelWaveletConvolution
  -> [VS.Vector (Complex Double)]
  -> IO [[VS.Vector (Complex Double)]]
applyPinwheelWaveletConvolution plan (Filter (PinwheelWaveletParams rows cols _ _ _ _ _) filters) xs = do
  ys <- dftExecuteBatch plan (DFTPlanID DFT2D [rows, cols] []) xs
  M.mapM
    (dftExecuteBatch plan (DFTPlanID IDFT2D [rows, cols] []) .
     L.concatMap (L.concatMap (\filter -> L.map (VS.zipWith (*) filter) ys)))
    filters
