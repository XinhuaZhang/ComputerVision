{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}

module CV.Filter.PinwheelFan
  ( module F
  , PinwheelFanExpansion
  , PinwheelFanConvolution
  , makePinwheelFanFilterExpansion
  , makePinwheelFanFilterConvolution
  , applyPinwheelFanFilterConvolution
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

newtype PinwheelFanExpansion =
  PinwheelFanExpansion (Filter PolarSeparableFilterParams [[[VU.Vector (Complex Double)]]])
newtype PinwheelFanConvolution =
  PinwheelFanConvolution (Filter PolarSeparableFilterParams [[[VS.Vector (Complex Double)]]])

{-# INLINE pinwheelFan #-}

pinwheelFan :: Double
            -> Double
            -> Double
            -> Int
            -> Double
            -> Int
            -> Int
            -> Complex Double
pinwheelFan gaussianScale waveletScale rf af thetaShift x y
  | x == 0 && y == 0 =
    1 / (2 * pi * sqrt waveletScale :+ 0) * (gaussian1D gaussianScale 0 :+ 0)
  | otherwise =
    1 / (2 * pi * sqrt waveletScale :+ 0) *
    (gaussian1D gaussianScale newDeltaTheta :+ 0) *
    exp (0 :+ fromIntegral af * newDeltaTheta) *
    exp (0 :+ rf * r)
  where
    r = log . sqrt . fromIntegral $ x ^ (2 :: Int) + y ^ (2 :: Int)
    theta = angleFunctionRad (fromIntegral x) (fromIntegral y)
    deltaTheta = (theta - thetaShift)
    newDeltaTheta =
      (L.minimumBy
         (\a b -> compare (abs a) (abs b))
         [deltaTheta, deltaTheta + 2 * pi, deltaTheta - 2 * pi]) /
      waveletScale


-- instance FilterExpansion PinwheelFanExpansion where
--   type FilterExpansionParameters PinwheelFanExpansion = PolarSeparableFilterParams
--   {-# INLINE makeFilterExpansion #-}
--   makeFilterExpansion params rCenter cCenter =
--     PinwheelFanExpansion . Filter params $
--     makePinwheelFanFilterExpansion params rCenter cCenter
--   {-# INLINE getFilterExpansionNum #-}
--   getFilterExpansionNum (PinwheelFanExpansion (Filter (PinwheelFanParams _ _ _ scales rfs afs radiuses) _)) =
--     L.length scales * L.length afs * L.length radiuses * L.length rfs
--   {-# INLINE applyFilterExpansion #-}
--   applyFilterExpansion (PinwheelFanExpansion (Filter _ filters)) =
--     L.concatMap
--       (\x ->
--          L.concatMap (L.concatMap (L.map (VU.sum . VU.zipWith (*) x))) filters)
--   {-# INLINE getFilterExpansionList #-}
--   getFilterExpansionList (PinwheelFanExpansion x) =
--     L.concatMap L.concat . getFilter $ x

-- instance FilterConvolution PinwheelFanConvolution where
--   type FilterConvolutionParameters PinwheelFanConvolution = PolarSeparableFilterParams
--   {-# INLINE makeFilterConvolution #-}
--   makeFilterConvolution plan params filterType =
--     second (PinwheelFanConvolution . Filter params) <$>
--     makePinwheelFanFilterConvolution plan params filterType
--   {-# INLINE getFilterConvolutionNum #-}
--   getFilterConvolutionNum (PinwheelFanConvolution (Filter (PinwheelFanParams _ _ _ scales rfs afs shifts) _)) =
--     L.length scales * L.length afs * L.length shifts * L.length rfs
--   {-# INLINE applyFilterConvolution #-}
--   applyFilterConvolution plan (PinwheelFanConvolution (Filter (PinwheelFanParams rows cols _ _ _ _ _) filters)) xs = do
--     ys <- dftExecuteBatch plan (DFTPlanID DFT2D [rows, cols] []) xs
--     dftExecuteBatch plan (DFTPlanID IDFT2D [rows, cols] []) .
--       L.concatMap
--         (\x -> L.concatMap (L.concatMap (L.map (VS.zipWith (*) x))) filters) $
--       ys
--   {-# INLINE applyInvariantFilterConvolution #-}
--   applyInvariantFilterConvolution plan (PinwheelFanConvolution (Filter (PinwheelFanParams rows cols _ _ _ _ _) filters)) xs = do
--     ys <- dftExecuteBatch plan (DFTPlanID DFT2D [rows, cols] []) xs
--     M.mapM
--       (dftExecuteBatch plan (DFTPlanID IDFT2D [rows, cols] []) .
--        L.concatMap (L.concatMap (\filter -> L.map (VS.zipWith (*) filter) ys)))
--       filters
--   {-# INLINE getFilterConvolutionList #-}
--   getFilterConvolutionList (PinwheelFanConvolution x) =
--     L.concatMap L.concat . getFilter $ x

{-# INLINE makePinwheelFanFilterExpansion #-}

makePinwheelFanFilterExpansion :: PolarSeparableFilterParams
                                       -> Int
                                       -> Int
                                       -> [[[VU.Vector (Complex Double)]]]
makePinwheelFanFilterExpansion params@(PinwheelFanParams rows cols gScale waveletScales rfs afs radiuses) rCenter cCenter =
  [ [ [ VU.fromListN (rows * cols) $
  makeFilterExpansionList
    rows
    cols
    rCenter
    cCenter
    (pinwheelFan gScale waveletScale rf af radius)
  | af <- afs
  , rf <- rfs
  ]
  | waveletScale <- waveletScales
  ]
  | radius <- radiuses
  ]
makePinwheelFanFilterExpansion _ _ _ =
  error "makePinwheelFanFilterExpansion: filter parameter type error."

{-# INLINE makePinwheelFanFilterConvolution #-}

makePinwheelFanFilterConvolution
  :: DFTPlan
  -> PolarSeparableFilterParams
  -> ConvolutionalFilterType
  -> IO (DFTPlan, [[[VS.Vector (Complex Double)]]])
makePinwheelFanFilterConvolution plan (PinwheelFanParams rows cols gScale waveletScales rfs afs shifts) filterType = do
  let filterList =
        L.map
          (\shift ->
             L.map
               (\waveletScale ->
                  L.map
                    (\(af, rf) ->
                       VS.fromList . conjugateFunc filterType $!
                       makeFilterConvolutionList
                         rows
                         cols
                         (pinwheelFan gScale waveletScale rf af shift)) $
                  [(af, rf) | af <- afs, rf <- rfs])
               waveletScales)
          shifts
      filterTmp =
        VS.fromListN (rows * cols) . conjugateFunc filterType $!
        makeFilterConvolutionList
          rows
          cols
          (pinwheelFan
             gScale
             (L.last waveletScales)
             (L.last rfs)
             (L.last afs)
             (L.last shifts))
  lock <- getFFTWLock
  (p1, vec) <- dft2dPlan lock plan rows cols filterTmp
  (p2, _) <- idft2dPlan lock p1 rows cols vec
  filters <-
    M.mapM
      (M.mapM (dftExecuteBatch p2 (DFTPlanID DFT2D [rows, cols] [])))
      filterList
  return (p2, filters)
makePinwheelFanFilterConvolution _ _ _ =
  error "makePinwheelFanFilterConvolution: filter parameter type error."


{-# INLINE applyPinwheelFanFilterConvolution #-}

applyPinwheelFanFilterConvolution
  :: DFTPlan
  -> Int
  -> Int
  -> [[[VS.Vector (Complex Double)]]]
  -> [VS.Vector (Complex Double)]
  -> IO [[VS.Vector (Complex Double)]]
applyPinwheelFanFilterConvolution plan rows cols filters xs = do
  ys <- dftExecuteBatch plan (DFTPlanID DFT2D [rows, cols] []) xs
  M.mapM
    (dftExecuteBatch plan (DFTPlanID IDFT2D [rows, cols] []) .
     L.concatMap (L.concatMap (\filter -> L.map (VS.zipWith (*) filter) ys)))
    filters
