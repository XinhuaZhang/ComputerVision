{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}
module CV.Filter.InverseGaussianPinwheel
  ( module F
  , InverseGaussianPinwheelExpansion
  , InverseGaussianPinwheelConvolution
  , makeInverseGaussianPinwheelFilterExpansion
  , makeInverseGaussianPinwheelFilterConvolution
  , makeInverseGaussianPinwheelFilterConvolutionPI
  , applyInverseGaussianPinwheelFilterConvolution
  ) where

import           Control.Arrow
import           Control.Monad                      as M
import           CV.Filter                          as F
import           CV.Filter.GaussianFilter
import           CV.Filter.PolarSeparableFilterType as F
import           CV.Utility.Coordinates
import           Data.Array.Repa                    as R
import           Data.Complex                       as C
import           Data.List                          as L
import           Data.Vector.Storable               as VS
import           Data.Vector.Unboxed                as VU



newtype InverseGaussianPinwheelExpansion = InverseGaussianPinwheelExpansion (Filter PolarSeparableFilterParams [[[VU.Vector (Complex Double)]]])
newtype InverseGaussianPinwheelConvolution = InverseGaussianPinwheelConvolution (Filter PolarSeparableFilterParams [[[VS.Vector (Complex Double)]]])

-- instance FilterExpansion InverseGaussianPinwheelExpansion where
--   type FilterExpansionParameters InverseGaussianPinwheelExpansion = PolarSeparableFilterParams
--   {-# INLINE makeFilterExpansion #-}
--   makeFilterExpansion params rCenter cCenter =
--     InverseGaussianPinwheelExpansion . Filter params $
--     makeInverseGaussianPinwheelFilterExpansion params rCenter cCenter
--   {-# INLINE getFilterExpansionNum #-}
--   getFilterExpansionNum (InverseGaussianPinwheelExpansion (Filter (InverseGaussianPinwheelParams _ _ scales rfs afs) _)) =
--     L.length scales * L.length rfs * L.length afs
--   {-# INLINE applyFilterExpansion #-}
--   applyFilterExpansion (InverseGaussianPinwheelExpansion (Filter _ filters)) =
--     L.concatMap
--       (\x ->
--          L.concatMap (L.concatMap (L.map (VU.sum . VU.zipWith (*) x))) filters)
--   {-# INLINE getFilterExpansionList #-}
--   getFilterExpansionList (InverseGaussianPinwheelExpansion x) =
--     L.concatMap L.concat . getFilter $ x


-- instance FilterConvolution InverseGaussianPinwheelConvolution where
--   type FilterConvolutionParameters InverseGaussianPinwheelConvolution = PolarSeparableFilterParams
--   {-# INLINE makeFilterConvolution #-}
--   makeFilterConvolution plan params@(InverseGaussianPinwheelParams rows cols scales rfs afs) filterType = do
--     second (InverseGaussianPinwheelConvolution . Filter params) <$>
--       makeInverseGaussianPinwheelFilterConvolution plan params filterType
--   {-# INLINE getFilterConvolutionNum #-}
--   getFilterConvolutionNum (InverseGaussianPinwheelConvolution (Filter (InverseGaussianPinwheelParams _ _ scales rfs afs) _)) =
--     L.length scales * L.length rfs * L.length afs
--   {-# INLINE applyFilterConvolution #-}
--   applyFilterConvolution plan (InverseGaussianPinwheelConvolution (Filter (InverseGaussianPinwheelParams rows cols _ _ _) filters)) xs = do
--     ys <- dftExecuteBatch plan (DFTPlanID DFT2D [rows, cols] []) xs
--     dftExecuteBatch plan (DFTPlanID IDFT2D [rows, cols] []) .
--       L.concatMap
--         (\x -> L.concatMap (L.concatMap (L.map (VS.zipWith (*) x))) filters) $
--       ys
--   {-# INLINE applyInvariantFilterConvolution #-}
--   applyInvariantFilterConvolution plan (InverseGaussianPinwheelConvolution (Filter (InverseGaussianPinwheelParams rows cols _ _ _) filters)) xs =
--     applyInverseGaussianPinwheelFilterConvolution plan rows cols filters xs
--   {-# INLINE getFilterConvolutionList #-}
--   getFilterConvolutionList (InverseGaussianPinwheelConvolution x) =
--     L.concatMap L.concat . getFilter $ x

{-# INLINE ejx #-}

ejx
  :: (RealFloat a)
  => a -> Complex a
ejx x = exp (0 :+ x)

{-# INLINE angularFunc #-}

angularFunc :: Int -> (Int -> Int -> Complex Double)
angularFunc freq x y =
  ejx (fromIntegral freq * angleFunctionRad (fromIntegral x) (fromIntegral y))

{-# INLINE radialFunc #-}

radialFunc :: Double -> Int -> Int -> (Int -> Int -> Complex Double)
radialFunc scale af rFreq x y
  | r == 0 = 1  -- 1
  | otherwise = exp (0 :+ fromIntegral rFreq * (log r))
  where
    r = sqrt . fromIntegral $ x ^ (2 :: Int) + y ^ (2 :: Int)

{-# INLINE pinwheels #-}

pinwheels :: Double
          -> Double
          -> Double
          -> Int
          -> Int
          -> (Int -> Int -> Complex Double)
pinwheels scale oscale iscale rf af x y
  | scale == 0 = angularFunc af x y * radialFunc scale af rf x y
  | oscale < iscale = error "pinwheels: oscale < iscale"
  | iscale == 0 =
    ((gaussian2D scale x y) :+ 0) * angularFunc af x y *
    radialFunc scale af rf x y
  | otherwise =
    ((gaussian2D scale x y) * (z - a) :+ 0) * angularFunc af x y *
    radialFunc scale af rf x y
  where
    z = gaussian2D oscale x y - gaussian2D iscale x y
    a = (1 / ((2 * pi) * oscale * oscale)) - (1 / ((2 * pi) * iscale * iscale))
    

{-# INLINE pinwheelsPI #-}

pinwheelsPI :: Double
          -> Double
          -> Double
          -> Int
          -> Int
          -> (Int -> Int -> Complex Double)
pinwheelsPI scale oscale iscale rf af x y
  | scale == 0 = angularFunc af x y * radialFunc scale af rf x y
  | oscale < iscale = error "pinwheelsPI: oscale < iscale"
  | iscale == 0 =
    ((gaussian2D scale x y) :+ 0) * angularFunc af x y *
    radialFunc scale af rf x y
  | otherwise =
    ((gaussian2D scale x y) * (z - a) :+ 0) * angularFunc af (-x) (-y) *
    radialFunc scale af rf x y
  where
    z = gaussian2D oscale x y - gaussian2D iscale x y
    a = (1 / ((2 * pi) * oscale * oscale)) - (1 / ((2 * pi) * iscale * iscale))



{-# INLINE makeInverseGaussianPinwheelFilterExpansion #-}

makeInverseGaussianPinwheelFilterExpansion :: PolarSeparableFilterParams -> Int -> Int -> [[[VU.Vector (Complex Double)]]]
makeInverseGaussianPinwheelFilterExpansion params@(InverseGaussianPinwheelParams rows cols scales rfs afs) rCenter cCenter =
  [ [ L.map
    (\af ->
       VU.fromList $
       makeFilterExpansionList
         rows
         cols
         rCenter
         cCenter
         (pinwheels
            scale
            (fromIntegral $ div (min rows cols) 2)
            (fromIntegral af)
            rf
            af)) .
  L.filter (\af -> fromIntegral (abs af) < (2 * scale)) $
  afs
  | rf <- rfs
  ]
  | scale <- scales
  ]
makeInverseGaussianPinwheelFilterExpansion _ _ _ =
  error "makeInverseGaussianPinwheelFilterExpansion: filter parameter type error."

{-# INLINE makeInverseGaussianPinwheelFilterConvolution #-}

makeInverseGaussianPinwheelFilterConvolution
  :: DFTPlan
  -> PolarSeparableFilterParams
  -> ConvolutionalFilterType
  -> IO (DFTPlan, [[[VS.Vector (Complex Double)]]])
makeInverseGaussianPinwheelFilterConvolution plan (InverseGaussianPinwheelParams rows cols scales rfs afs) filterType = do
  let filterList =
        L.map
          (\scale ->
             L.map
               (\af ->
                  L.map
                    (\rf ->
                       VS.fromList .
                       conjugateFunc filterType .
                       makeFilterConvolutionList rows cols $
                       pinwheels
                         scale
                         (fromIntegral $ div (min rows cols) 2)
                         (fromIntegral . abs $ af)
                         rf
                         af)
                    rfs) .
             L.filter (\af -> fromIntegral (abs af) < (2 * scale)) $
             afs)
          scales
      filterTemp =
        VS.fromListN (rows * cols) .
        conjugateFunc filterType . makeFilterConvolutionList rows cols $
        pinwheels
          (L.last scales)
          (fromIntegral $ div (min rows cols) 2)
          (fromIntegral $ L.last afs)
          (L.last rfs)
          (L.last afs)
  lock <- getFFTWLock
  (p1, vec) <- dft2dPlan lock plan rows cols filterTemp
  (p2, _) <- idft2dPlan lock p1 rows cols vec
  filters <-
    M.mapM
      (M.mapM (dftExecuteBatch p2 (DFTPlanID DFT2D [rows, cols] [])))
      filterList
  return (p2, filters)
makeInverseGaussianPinwheelFilterConvolution _ _ _ =
  error "makeInverseGaussianPinwheelFilterConvolution: filter parameter type error."

{-# INLINE makeInverseGaussianPinwheelFilterConvolutionPI #-}

makeInverseGaussianPinwheelFilterConvolutionPI
  :: DFTPlan
  -> PolarSeparableFilterParams
  -> ConvolutionalFilterType
  -> IO (DFTPlan, [[[VS.Vector (Complex Double)]]])
makeInverseGaussianPinwheelFilterConvolutionPI plan (InverseGaussianPinwheelParams rows cols scales rfs afs) filterType = do
  let filterList =
        L.map
          (\scale ->
             L.map
               (\af ->
                  L.map
                    (\rf ->
                       VS.fromList .
                       conjugateFunc filterType .
                       makeFilterConvolutionList rows cols $
                       pinwheelsPI
                         scale
                         (fromIntegral $ div (min rows cols) 2)
                         (fromIntegral . abs $ af)
                         rf
                         af)
                    rfs) .
             L.filter (\af -> fromIntegral (abs af) < (2 * scale)) $
             afs)
          scales
      filterTemp =
        VS.fromListN (rows * cols) .
        conjugateFunc filterType . makeFilterConvolutionList rows cols $
        pinwheelsPI
          (L.last scales)
          (fromIntegral $ div (min rows cols) 2)
          (fromIntegral $ L.last afs)
          (L.last rfs)
          (L.last afs)
  lock <- getFFTWLock
  (p1, vec) <- dft2dPlan lock plan rows cols filterTemp
  (p2, _) <- idft2dPlan lock p1 rows cols vec
  filters <-
    M.mapM
      (M.mapM (dftExecuteBatch p2 (DFTPlanID DFT2D [rows, cols] [])))
      filterList
  return (p2, filters)
makeInverseGaussianPinwheelFilterConvolutionPI _ _ _ =
  error "makeInverseGaussianPinwheelFilterConvolutionPI: filter parameter type error."


{-# INLINE applyInverseGaussianPinwheelFilterConvolution #-}

applyInverseGaussianPinwheelFilterConvolution
  :: DFTPlan
  -> Int
  -> Int
  -> [[[VS.Vector (Complex Double)]]]
  -> [VS.Vector (Complex Double)]
  -> IO [[VS.Vector (Complex Double)]]
applyInverseGaussianPinwheelFilterConvolution plan rows cols filters xs = do
  ys <- dftExecuteBatch plan (DFTPlanID DFT2D [rows, cols] []) xs
  fmap (\x -> [x]) .
    dftExecuteBatch plan (DFTPlanID IDFT2D [rows, cols] []) .
    L.concatMap
      (\x -> L.concatMap (L.concatMap (L.map (VS.zipWith (*) x))) filters) $
    ys
