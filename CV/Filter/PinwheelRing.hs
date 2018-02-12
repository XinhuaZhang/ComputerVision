{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}

module CV.Filter.PinwheelRing
  ( module F
  , PinwheelRingExpansion
  , PinwheelRingConvolution
  , applyPinwheelRingFilterConvolution
  , makePinwheelRingFilterExpansion
  , makePinwheelRingFilterConvolution
  , makePinwheelRingFilterConvolutionPI
  -- , applyPinwheelRingExpansion
  ) where

import           Control.Arrow
import           Control.Monad                      as M
import           CV.Filter                          as F
import           CV.Filter.GaussianFilter
import           CV.Filter.PolarSeparableFilterType as F
import           CV.Utility.Coordinates
import           CV.Utility.DFT
import           CV.Utility.Parallel
import           Data.Complex                       as C
import           Data.List                          as L
import           Data.Vector.Storable               as VS
import           Data.Vector.Unboxed                as VU

newtype PinwheelRingExpansion =
  PinwheelRingExpansion (Filter PolarSeparableFilterParams [[[VU.Vector (Complex Double)]]])
newtype PinwheelRingConvolution =
  PinwheelRingConvolution (Filter PolarSeparableFilterParams [[[VS.Vector (Complex Double)]]])

{-# INLINE pinwheelRing #-}

pinwheelRing :: Double
             -> Double
             -> Double
             -> Int
             -> Double
             -> Int
             -> Int
             -> Complex Double
pinwheelRing gaussianScale waveletScale rf af shift x y
  | x == 0 && y == 0 && shift == 0 =
    1 / (2 * pi * sqrt waveletScale :+ 0) * (gaussian1D gaussianScale 0 :+ 0)
  | x == 0 && y == 0 && shift /= 0 =
    1 / (2 * pi * sqrt waveletScale :+ 0) *
    (gaussian1D gaussianScale (-r') :+ 0)
  | otherwise =
    1 / (2 * pi * sqrt waveletScale :+ 0) *
    (gaussian1D gaussianScale (r - r') :+ 0) *
    exp
      (0 :+
       (fromIntegral af * angleFunctionRad (fromIntegral x) (fromIntegral y))) *
    exp (0 :+ rf * (r - r')) /
    (cr :+ 0)
  where
    cr = sqrt . fromIntegral $ x ^ (2 :: Int) + y ^ (2 :: Int)
    r =
      (log . sqrt . fromIntegral $ x ^ (2 :: Int) + y ^ (2 :: Int)) /
      waveletScale
    r' =
      if shift == 0
        then 0
        else (log shift) -- / waveletScale
        
{-# INLINE pinwheelRingPI #-}

pinwheelRingPI :: Double
             -> Double
             -> Double
             -> Int
             -> Double
             -> Int
             -> Int
             -> Complex Double
pinwheelRingPI gaussianScale waveletScale rf af shift x y
  | x == 0 && y == 0 && shift == 0 =
    0 -- 1 / (2 * pi * sqrt waveletScale :+ 0) * (gaussian1D gaussianScale 0 :+ 0)
  | x == 0 && y == 0 && shift /= 0 = 0
  | otherwise =
    1 / (2 * pi * sqrt waveletScale :+ 0) *
    (gaussian1D gaussianScale (r - r') :+ 0) *
    exp
      (0 :+ fromIntegral af * angleFunctionRad (fromIntegral (-x)) (fromIntegral (-y))) *
    exp (0 :+ rf * (r - r')) /
    (cr :+ 0)
  where
    cr = sqrt . fromIntegral $ x ^ (2 :: Int) + y ^ (2 :: Int)
    r =
      (log . sqrt . fromIntegral $ x ^ (2 :: Int) + y ^ (2 :: Int)) /
      waveletScale
    r' =
      if shift == 0
        then 0
        else (log shift) -- / waveletScale

-- instance FilterExpansion PinwheelRingExpansion where
--   type FilterExpansionParameters PinwheelRingExpansion = PolarSeparableFilterParams
--   {-# INLINE makeFilterExpansion #-}
--   makeFilterExpansion params rCenter cCenter =
--     PinwheelRingExpansion . Filter params $
--     makePinwheelRingFilterExpansion params rCenter cCenter
--   {-# INLINE getFilterExpansionNum #-}
--   getFilterExpansionNum (PinwheelRingExpansion (Filter (PinwheelRingParams _ _ _ scales rfs afs radiuses) _)) =
--     L.length scales * L.length afs * L.length radiuses * L.length rfs
--   {-# INLINE applyFilterExpansion #-}
--   applyFilterExpansion (PinwheelRingExpansion (Filter _ filters)) =
--     L.concatMap
--       (\x ->
--          L.concatMap (L.concatMap (L.map (VU.sum . VU.zipWith (*) x))) filters)
--   {-# INLINE getFilterExpansionList #-}
--   getFilterExpansionList (PinwheelRingExpansion x) =
--     L.concatMap L.concat . getFilter $ x


-- instance FilterConvolution PinwheelRingConvolution where
--   type FilterConvolutionParameters PinwheelRingConvolution = PolarSeparableFilterParams
--   {-# INLINE makeFilterConvolution #-}
--   makeFilterConvolution plan params filterType =
--     second (PinwheelRingConvolution . Filter params) <$>
--     makePinwheelRingFilterConvolution plan params filterType
--   {-# INLINE getFilterConvolutionNum #-}
--   getFilterConvolutionNum (PinwheelRingConvolution (Filter (PinwheelRingParams _ _ _ scales rfs afs radiuses) _)) =
--     L.length scales * L.length afs * L.length radiuses * L.length rfs
--   {-# INLINE applyFilterConvolution #-}
--   applyFilterConvolution plan (PinwheelRingConvolution (Filter (PinwheelRingParams rows cols _ _ _ _ _) filters)) xs = do
--     ys <- dftExecuteBatch plan (DFTPlanID DFT2D [rows, cols] []) xs
--     dftExecuteBatch plan (DFTPlanID IDFT2D [rows, cols] []) .
--       L.concatMap
--         (\x -> L.concatMap (L.concatMap (L.map (VS.zipWith (*) x))) filters) $
--       ys
--   {-# INLINE applyInvariantFilterConvolution #-}
--   applyInvariantFilterConvolution plan (PinwheelRingConvolution (Filter (PinwheelRingParams rows cols _ _ _ _ _) filters)) xs = do
--     ys <- dftExecuteBatch plan (DFTPlanID DFT2D [rows, cols] []) xs
--     M.mapM
--       (dftExecuteBatch plan (DFTPlanID IDFT2D [rows, cols] []) .
--        L.concatMap (L.concatMap (\filter -> L.map (VS.zipWith (*) filter) ys)))
--       filters
--   {-# INLINE getFilterConvolutionList #-}
--   getFilterConvolutionList (PinwheelRingConvolution x) =
--     L.concatMap L.concat . getFilter $ x

-- {-# INLINE applyPinwheelRingExpansion #-}

-- applyPinwheelRingExpansion
--   :: PinwheelRingExpansion
--   -> [VU.Vector (Complex Double)]
--   -> [[[[Complex Double]]]]
-- applyPinwheelRingExpansion (Filter _ pFilters) imgVecs =
--   L.map
--     (\scales ->
--         L.concatMap
--           (\imgVec ->
--               parMap rdeepseq (L.map (L.map (VU.sum . VU.zipWith (*) imgVec))) scales)
--           imgVecs)
--     pFilters

-- applyPinwheelRingExpansion
--   :: PinwheelRingExpansion
--   -> [VU.Vector (Complex Double)]
--   -> [[[Complex Double]]]
-- applyPinwheelRingExpansion (Filter _ pFilters) imgVecs =
--   L.map
--     (\scales ->
--         L.concatMap
--           (\imgVec ->
--               parMap rdeepseq (L.map (VU.sum . VU.zipWith (*) imgVec)) . L.concat $
--               scales)
--           imgVecs)
--     pFilters

{-# INLINE makePinwheelRingFilterExpansion #-}

makePinwheelRingFilterExpansion :: PolarSeparableFilterParams
                                       -> Int
                                       -> Int
                                       -> [[[VU.Vector (Complex Double)]]]
makePinwheelRingFilterExpansion params@(PinwheelRingParams rows cols gScale waveletScales rfs afs radiuses) rCenter cCenter =
  [ [ [ VU.fromList $
  makeFilterExpansionList
    rows
    cols
    rCenter
    cCenter
    (pinwheelRing gScale waveletScale rf af radius)
  | af <- afs
  , rf <- rfs
  ]
  | waveletScale <- waveletScales
  ]
  | radius <- radiuses
  ]
makePinwheelRingFilterExpansion _ _ _ =
  error "makePinwheelRingFilterExpansion: filter parameter type error."

{-# INLINE makePinwheelRingFilterConvolution #-}

makePinwheelRingFilterConvolution
  :: DFTPlan
  -> PolarSeparableFilterParams
  -> ConvolutionalFilterType
  -> IO (DFTPlan, [[[VS.Vector (Complex Double)]]])
makePinwheelRingFilterConvolution plan (PinwheelRingParams rows cols gScale waveletScales rfs afs radiuses) filterType = do
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
                         (pinwheelRing gScale waveletScale rf af radius)) $
                  [(af, rf) | af <- afs, rf <- rfs])
               waveletScales)
          radiuses
      filterTmp =
        VS.fromListN (rows * cols) . conjugateFunc filterType $!
        makeFilterConvolutionList
          rows
          cols
          (pinwheelRing
             gScale
             (L.last waveletScales)
             (L.last rfs)
             (L.last afs)
             (L.last radiuses))
  lock <- getFFTWLock
  (p1, vec) <- dft2dPlan lock plan rows cols filterTmp
  (p2, _) <- idft2dPlan lock p1 rows cols vec
  filters <-
    M.mapM
      (M.mapM (dftExecuteBatch p2 (DFTPlanID DFT2D [rows, cols] [])))
      filterList
  return (p2, filters)
makePinwheelRingFilterConvolution _ _ _ =
  error "makePinwheelRingFilterConvolution: filter parameter type error."
  
{-# INLINE makePinwheelRingFilterConvolutionPI #-}

makePinwheelRingFilterConvolutionPI
  :: DFTPlan
  -> PolarSeparableFilterParams
  -> ConvolutionalFilterType
  -> IO (DFTPlan, [[[VS.Vector (Complex Double)]]])
makePinwheelRingFilterConvolutionPI plan (PinwheelRingParams rows cols gScale waveletScales rfs afs radiuses) filterType = do
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
                         (pinwheelRingPI gScale waveletScale rf af radius)) $
                  [(af, rf) | af <- afs, rf <- rfs])
               waveletScales)
          radiuses
      filterTmp =
        VS.fromListN (rows * cols) . conjugateFunc filterType $!
        makeFilterConvolutionList
          rows
          cols
          (pinwheelRingPI
             gScale
             (L.last waveletScales)
             (L.last rfs)
             (L.last afs)
             (L.last radiuses))
  lock <- getFFTWLock
  (p1, vec) <- dft2dPlan lock plan rows cols filterTmp
  (p2, _) <- idft2dPlan lock p1 rows cols vec
  filters <-
    M.mapM
      (M.mapM (dftExecuteBatch p2 (DFTPlanID DFT2D [rows, cols] [])))
      filterList
  return (p2, filters)
makePinwheelRingFilterConvolutionPI _ _ _ =
  error "makePinwheelRingFilterConvolutionPI: filter parameter type error."

{-# INLINE applyPinwheelRingFilterConvolution #-}

applyPinwheelRingFilterConvolution
  :: DFTPlan -> Int -> Int
  -> [[[VS.Vector (Complex Double)]]]
  -> [VS.Vector (Complex Double)]
  -> IO [[VS.Vector (Complex Double)]]
applyPinwheelRingFilterConvolution plan rows cols filters xs = do
  ys <- dftExecuteBatch plan (DFTPlanID DFT2D [rows, cols] []) xs
  M.mapM
    (dftExecuteBatch plan (DFTPlanID IDFT2D [rows, cols] []) .
     L.concatMap (L.concatMap (\filter -> L.map (VS.zipWith (*) filter) ys)))
    filters
