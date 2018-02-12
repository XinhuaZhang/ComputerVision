{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}
module CV.Filter.GaussianPinwheel
  ( module F
  , GaussianPinwheelExpansion
  , GaussianPinwheelConvolution
  , makeGaussianPinwheelFilterExpansion
  , makeGaussianPinwheelFilterConvolution
  , makeGaussianPinwheelFilterConvolutionPI
  , applyGaussianPinwheelFilterConvolution
  ) where

import           Control.Arrow
import           Control.Monad                      as M
import           CV.Filter                          as F
import           CV.Filter.PolarSeparableFilterType as F
import           CV.Utility.Coordinates
import           Data.Array.Repa                    as R
import           Data.Complex                       as C
import           Data.List                          as L
import           Data.Vector.Storable               as VS
import           Data.Vector.Unboxed                as VU



newtype GaussianPinwheelExpansion = GaussianPinwheelExpansion (Filter PolarSeparableFilterParams [[[VU.Vector (Complex Double)]]])
newtype GaussianPinwheelConvolution = GaussianPinwheelConvolution (Filter PolarSeparableFilterParams [[[VS.Vector (Complex Double)]]])

-- instance FilterExpansion GaussianPinwheelExpansion where
--   type FilterExpansionParameters GaussianPinwheelExpansion = PolarSeparableFilterParams
--   {-# INLINE makeFilterExpansion #-}
--   makeFilterExpansion params rCenter cCenter =
--     GaussianPinwheelExpansion . Filter params $
--     makeGaussianPinwheelFilterExpansion params rCenter cCenter
--   {-# INLINE getFilterExpansionNum #-}
--   getFilterExpansionNum (GaussianPinwheelExpansion (Filter (GaussianPinwheelParams _ _ scales rfs afs) _)) =
--     L.length scales * L.length rfs * L.length afs
--   {-# INLINE applyFilterExpansion #-}
--   applyFilterExpansion (GaussianPinwheelExpansion (Filter _ filters)) =
--     L.concatMap
--       (\x ->
--          L.concatMap (L.concatMap (L.map (VU.sum . VU.zipWith (*) x))) filters)
--   {-# INLINE getFilterExpansionList #-}
--   getFilterExpansionList (GaussianPinwheelExpansion x) =
--     L.concatMap L.concat . getFilter $ x


-- instance FilterConvolution GaussianPinwheelConvolution where
--   type FilterConvolutionParameters GaussianPinwheelConvolution = PolarSeparableFilterParams
--   {-# INLINE makeFilterConvolution #-}
--   makeFilterConvolution plan params@(GaussianPinwheelParams rows cols scales rfs afs) filterType = do
--     second (GaussianPinwheelConvolution . Filter params) <$>
--       makeGaussianPinwheelFilterConvolution plan params filterType
--   {-# INLINE getFilterConvolutionNum #-}
--   getFilterConvolutionNum (GaussianPinwheelConvolution (Filter (GaussianPinwheelParams _ _ scales rfs afs) _)) =
--     L.length scales * L.length rfs * L.length afs
--   {-# INLINE applyFilterConvolution #-}
--   applyFilterConvolution plan (GaussianPinwheelConvolution (Filter (GaussianPinwheelParams rows cols _ _ _) filters)) xs = do
--     ys <- dftExecuteBatch plan (DFTPlanID DFT2D [rows, cols] []) xs
--     dftExecuteBatch plan (DFTPlanID IDFT2D [rows, cols] []) .
--       L.concatMap
--         (\x -> L.concatMap (L.concatMap (L.map (VS.zipWith (*) x))) filters) $
--       ys
--   {-# INLINE applyInvariantFilterConvolution #-}
--   applyInvariantFilterConvolution plan (GaussianPinwheelConvolution (Filter (GaussianPinwheelParams rows cols _ _ _) filters)) xs =
--     applyGaussianPinwheelFilterConvolution plan rows cols filters xs
--   {-# INLINE getFilterConvolutionList #-}
--   getFilterConvolutionList (GaussianPinwheelConvolution x) =
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
  | r == 0 = 1
  | otherwise = exp (0 :+ fromIntegral rFreq * (log r))
  where
    r = sqrt . fromIntegral $ x ^ (2 :: Int) + y ^ (2 :: Int)

{-# INLINE pinwheels #-}

pinwheels :: Double -> Int -> Int -> (Int -> Int -> Complex Double)
pinwheels scale rf af x y
  | scale == 0 = angularFunc af x y * radialFunc scale af rf x y
  | otherwise =
    (gaussian2D'' rf scale x y :+ 0) * angularFunc af x y *
    radialFunc scale af rf x y
    

{-# INLINE radialFunc' #-}

radialFunc' :: Double -> Int -> Int -> (Int -> Int -> Complex Double)
radialFunc' scale af rFreq x y
  | r == 0 = 0
  | otherwise = exp (0 :+ fromIntegral rFreq * (log r))
  where
    r = sqrt . fromIntegral $ x ^ (2 :: Int) + y ^ (2 :: Int)

{-# INLINE pinwheels' #-}

pinwheels' :: Double -> Int -> Int -> (Int -> Int -> Complex Double)
pinwheels' scale rf af x y
  | scale == 0 = angularFunc af x y * radialFunc scale af rf x y
  | otherwise =
    (gaussian2D'' rf scale x y :+ 0) * angularFunc af x y *
    radialFunc' scale af rf x y


{-# INLINE pinwheelsPI #-}

pinwheelsPI :: Double -> Int -> Int -> (Int -> Int -> Complex Double)
pinwheelsPI scale rf af x y
  | scale == 0 = angularFunc af x y * radialFunc scale af rf x y
  | otherwise =
    (gaussian2D'' rf scale x y :+ 0) * angularFunc af (-x) (-y) *
    radialFunc scale af rf x y


{-# INLINE gaussian2D'' #-}
gaussian2D''
  :: (Floating a, Ord a)
    => Int -> a -> Int -> Int -> a
gaussian2D'' freq sd i j -- =
  | r == 0 = 1 / ((2 * pi) * sd * sd)
  | otherwise =
    1 / ((2 * pi) * sd * sd) *
    exp (-(log r) ^ (2 :: Int) / (2 * (sd ^ (2 :: Int))))
  where
    r = sqrt $ fromIntegral (i * i + j * j)
    
{-# INLINE makeGaussianPinwheelFilterExpansion #-}

makeGaussianPinwheelFilterExpansion :: PolarSeparableFilterParams -> Int -> Int -> [[[VU.Vector (Complex Double)]]]
makeGaussianPinwheelFilterExpansion params@(GaussianPinwheelParams rows cols scales rfs afs) rCenter cCenter =
  [ [ [ VU.fromList $
  makeFilterExpansionList rows cols rCenter cCenter (pinwheels scale rf af)
  | af <- afs
  ]
  | rf <- rfs
  ]
  | scale <- scales
  ]
makeGaussianPinwheelFilterExpansion _ _ _ =
  error "makeGaussianPinwheelFilterExpansion: filter parameter type error."
  
{-# INLINE makeGaussianPinwheelFilterConvolution #-}

makeGaussianPinwheelFilterConvolution
  :: DFTPlan
  -> PolarSeparableFilterParams
  -> ConvolutionalFilterType
  -> IO (DFTPlan, [[[VS.Vector (Complex Double)]]])
makeGaussianPinwheelFilterConvolution plan (GaussianPinwheelParams rows cols scales rfs afs) filterType = do
  let filterList =
        L.map
          (\scale ->
             L.map
               (\af ->
                  (L.map
                     (\rf ->
                        VS.fromList .
                        conjugateFunc filterType .
                        makeFilterConvolutionList rows cols $
                        pinwheels scale rf af)
                     rfs) -- L.++
                  -- (L.map
                  --    (\rf ->
                  --       VS.fromList .
                  --       conjugateFunc filterType .
                  --       makeFilterConvolutionList rows cols $
                  --       pinwheels' scale rf af)
                  --    rfs)
               )
               afs)
          scales
      filterTemp =
        VS.fromListN (rows * cols) .
        conjugateFunc filterType . makeFilterConvolutionList rows cols $
        pinwheels (L.last scales) (L.last rfs) (L.last afs)
  lock <- getFFTWLock
  (p1, vec) <- dft2dPlan lock plan rows cols filterTemp
  (p2, _) <- idft2dPlan lock p1 rows cols vec
  filters <-
    M.mapM
      (M.mapM (dftExecuteBatch p2 (DFTPlanID DFT2D [rows, cols] [])))
      filterList
  return (p2, filters)
makeGaussianPinwheelFilterConvolution _ _ _ =
  error "makeGaussianPinwheelFilterConvolution: filter parameter type error."
  
{-# INLINE makeGaussianPinwheelFilterConvolutionPI #-}

makeGaussianPinwheelFilterConvolutionPI
  :: DFTPlan
  -> PolarSeparableFilterParams
  -> ConvolutionalFilterType
  -> IO (DFTPlan, [[[VS.Vector (Complex Double)]]])
makeGaussianPinwheelFilterConvolutionPI plan (GaussianPinwheelParams rows cols scales rfs afs) filterType = do
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
                       pinwheelsPI scale rf af)
                    rfs)
               afs)
          scales
      filterTemp =
        VS.fromListN (rows * cols) .
        conjugateFunc filterType . makeFilterConvolutionList rows cols $
        pinwheelsPI (L.last scales) (L.last rfs) (L.last afs)
  lock <- getFFTWLock
  (p1, vec) <- dft2dPlan lock plan rows cols filterTemp
  (p2, _) <- idft2dPlan lock p1 rows cols vec
  filters <-
    M.mapM
      (M.mapM (dftExecuteBatch p2 (DFTPlanID DFT2D [rows, cols] [])))
      filterList
  return (p2, filters)
makeGaussianPinwheelFilterConvolutionPI _ _ _ =
  error "makeGaussianPinwheelFilterConvolutionPI: filter parameter type error."


{-# INLINE applyGaussianPinwheelFilterConvolution #-}

applyGaussianPinwheelFilterConvolution
  :: DFTPlan
  -> Int
  -> Int
  -> [[[VS.Vector (Complex Double)]]]
  -> [VS.Vector (Complex Double)]
  -> IO [[VS.Vector (Complex Double)]]
applyGaussianPinwheelFilterConvolution plan rows cols filters xs = do
  ys <- dftExecuteBatch plan (DFTPlanID DFT2D [rows, cols] []) xs
  fmap (\x -> [x]) .
    dftExecuteBatch plan (DFTPlanID IDFT2D [rows, cols] []) .
    L.concatMap
      (\x -> L.concatMap (L.concatMap (L.map (VS.zipWith (*) x))) filters) $
    ys
