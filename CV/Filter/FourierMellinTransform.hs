{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}
module CV.Filter.FourierMellinTransform
  ( module F
  , FourierMellinTransformExpansion
  , FourierMellinTransformConvolution
  , fourierMellinTransform
  , makeFourierMellinTransformFilterExpansion
  , makeFourierMellinTransformFilterConvolution
  , makeFourierMellinTransformFilterConvolutionPlan
  , makeFourierMellinTransformFilterConvolutionFilter
  , makeFourierMellinTransformFilterConvolutionPI
  , applyFourierMellinTransformFilterConvolution
  ) where

import           Control.Arrow
import           Control.Monad                      as M
import           CV.Filter                          as F
import           CV.Filter.PolarSeparableFilterType as F
import           CV.Filter.GaussianFilter
import           CV.Utility.Coordinates
import           Data.Complex                       as C
import           Data.List                          as L
import           Data.Vector.Storable               as VS
import           Data.Vector.Unboxed                as VU


newtype FourierMellinTransformExpansion =
  FourierMellinTransformExpansion (Filter PolarSeparableFilterParams [[VU.Vector (Complex Double)]])
newtype FourierMellinTransformConvolution =
  FourierMellinTransformConvolution (Filter PolarSeparableFilterParams [[VS.Vector (Complex Double)]])

-- instance FilterExpansion FourierMellinTransformExpansion where
--   type FilterExpansionParameters FourierMellinTransformExpansion = PolarSeparableFilterParams
--   {-# INLINE makeFilterExpansion #-}
--   makeFilterExpansion params@(FourierMellinTransformParams rows cols rfs afs) rCenter cCenter =
--     FourierMellinTransformExpansion . Filter params $
--     makeFourierMellinTransformFilterExpansion params rCenter cCenter
--   {-# INLINE getFilterExpansionNum #-}
--   getFilterExpansionNum (FourierMellinTransformExpansion (Filter (FourierMellinTransformParams _ _ rfs afs) _)) =
--     L.length rfs * L.length afs
--   {-# INLINE applyFilterExpansion #-}
--   applyFilterExpansion (FourierMellinTransformExpansion (Filter _ filters)) =
--     L.concatMap (\x -> L.concatMap (L.map (VU.sum . VU.zipWith (*) x)) filters)
--   {-# INLINE getFilterExpansionList #-}
--   getFilterExpansionList (FourierMellinTransformExpansion x) =
--     L.concat . getFilter $ x


-- instance FilterConvolution FourierMellinTransformConvolution where
--   type FilterConvolutionParameters FourierMellinTransformConvolution = PolarSeparableFilterParams
--   {-# INLINE makeFilterConvolution #-}
--   makeFilterConvolution plan params filterType = do
--     second (FourierMellinTransformConvolution . Filter params) <$>
--       makeFourierMellinTransformFilterConvolution plan params filterType
--     error
--       "makeFilterConvolution: FourierMellinTransform: filter parameter type error."
--   {-# INLINE getFilterConvolutionNum #-}
--   getFilterConvolutionNum (FourierMellinTransformConvolution (Filter (FourierMellinTransformParams _ _ rfs afs) _)) =
--     L.length rfs * L.length afs
--   {-# INLINE applyFilterConvolution #-}
--   applyFilterConvolution plan (FourierMellinTransformConvolution (Filter (FourierMellinTransformParams rows cols _ _) filters)) xs = do
--     ys <- dftExecuteBatch plan (DFTPlanID DFT2D [rows, cols] []) xs
--     dftExecuteBatch plan (DFTPlanID IDFT2D [rows, cols] []) .
--       L.concatMap (\x -> L.concatMap (L.map (VS.zipWith (*) x)) filters) $
--       ys
--   {-# INLINE applyInvariantFilterConvolution #-}
--   applyInvariantFilterConvolution plan (FourierMellinTransformConvolution (Filter (FourierMellinTransformParams rows cols _ _) filters)) xs =
--     applyFourierMellinTransformFilterConvolution plan rows cols filters xs
--   {-# INLINE getFilterConvolutionList #-}
--   getFilterConvolutionList (FourierMellinTransformConvolution x) =
--     L.concat . getFilter $ x

{-# INLINE fourierMellinTransform #-}

fourierMellinTransform :: Double
                       -> Int
                       -> Double
                       -> Int
                       -> Int
                       -> Complex Double
fourierMellinTransform rf af alpha x y
  | r <= (6 / pi * (fromIntegral . abs $ af)) = 0
  -- | x == 0 && y == 0 = 0
  | otherwise =
    (((r ) :+ 0) ** (alpha :+ (-rf))) * exp (0 :+ (fromIntegral (-af) * theta))
  where
    r = sqrt . fromIntegral $ x ^ (2 :: Int) + y ^ (2 :: Int) 
    theta = angleFunctionRad (fromIntegral x) (fromIntegral y)


{-# INLINE fourierMellinTransformPI #-}

fourierMellinTransformPI :: Double
                         -> Int
                         -> Double
                         -> Int
                         -> Int
                         -> Complex Double
fourierMellinTransformPI rf af alpha x y
  | r <= (6 / pi * (fromIntegral . abs $ af)) = 0
  -- | x == 0 && y == 0 = 0
  | otherwise =
    ((r :+ 0) ** (alpha :+ (-rf))) * exp (0 :+ (fromIntegral (-af) * theta))
  where
    r = sqrt . fromIntegral $ x ^ (2 :: Int) + y ^ (2 :: Int)
    theta = angleFunctionRad (fromIntegral (-x)) (fromIntegral (-y))

{-# INLINE makeFourierMellinTransformFilterExpansion #-}

makeFourierMellinTransformFilterExpansion :: PolarSeparableFilterParams
                                          -> Int
                                          -> Int
                                          -> [[VU.Vector (Complex Double)]]
makeFourierMellinTransformFilterExpansion (FourierMellinTransformParams rows cols rfs afs alpha) rCenter cCenter =
  [ [ VU.fromList $
  makeFilterExpansionList
    rows
    cols
    rCenter
    cCenter
    (fourierMellinTransform rf af alpha)
  | af <- afs
  ]
  | rf <- rfs
  ]
makeFourierMellinTransformFilterExpansion _ _ _ =
  error
    "makeFourierMellinTransformFilterExpansion : filter parameter type error."

{-# INLINE makeFourierMellinTransformFilterConvolution #-}

makeFourierMellinTransformFilterConvolution
  :: DFTPlan
  -> PolarSeparableFilterParams
  -> ConvolutionalFilterType
  -> IO (DFTPlan, [[VS.Vector (Complex Double)]])
makeFourierMellinTransformFilterConvolution plan (FourierMellinTransformParams rows cols rfs afs alpha) filterType = do
  let filterTemp =
        VS.fromList . conjugateFunc filterType $!
        makeFilterConvolutionList
          rows
          cols
          (fourierMellinTransform (L.last rfs) (L.last afs) alpha)
      filterList =
        L.map
          (\rf ->
             L.map
               (\af ->
                  VS.fromList . conjugateFunc filterType $!
                  makeFilterConvolutionList
                    rows
                    cols
                    (fourierMellinTransform rf af alpha))
               afs)
          rfs
  lock <- getFFTWLock
  (p1, vec) <- dft2dPlan lock plan rows cols filterTemp
  (p2, _) <- idft2dPlan lock p1 rows cols vec
  filters <-
    M.mapM (dftExecuteBatch p2 (DFTPlanID DFT2D [rows, cols] [])) filterList
  return (p2, filters)
makeFourierMellinTransformFilterConvolution _ _ _ =
  error
    "makeFourierMellinTransformFilterConvolution: filter parameter type error."
    

{-# INLINE makeFourierMellinTransformFilterConvolutionPlan #-}

makeFourierMellinTransformFilterConvolutionPlan
  :: DFTPlan
  -> PolarSeparableFilterParams
  -> ConvolutionalFilterType
  -> [(Int, Int)]
  -> IO DFTPlan
makeFourierMellinTransformFilterConvolutionPlan plan (FourierMellinTransformParams _ _ rfs afs alpha) filterType dims = do
  lock <- getFFTWLock
  M.foldM
    (\p (rows, cols) -> do
       let filterTemp =
             VS.fromList . conjugateFunc filterType $!
             makeFilterConvolutionList
               rows
               cols
               (fourierMellinTransform (L.last rfs) (L.last afs) alpha)
       case F.lookup (DFTPlanID DFT2D [rows, cols] []) p of
         Nothing -> do
           (p1, vec) <- dft2dPlan lock p rows cols filterTemp
           (p2, _) <- idft2dPlan lock p1 rows cols vec
           return p2
         Just _ -> return p)
    plan
    dims
makeFourierMellinTransformFilterConvolutionPlan _ _ _ _ =
  error
    "makeFourierMellinTransformFilterConvolutionPlan: filter parameter type error."
    
{-# INLINE makeFourierMellinTransformFilterConvolutionFilter #-}

makeFourierMellinTransformFilterConvolutionFilter
  :: DFTPlan
  -> PolarSeparableFilterParams
  -> ConvolutionalFilterType
  -> IO [[VS.Vector (Complex Double)]]
makeFourierMellinTransformFilterConvolutionFilter plan (FourierMellinTransformParams rows cols rfs afs alpha) filterType = do
  let filterList =
        L.map
          (\rf ->
             L.map
               (\af ->
                  VS.fromList . conjugateFunc filterType $!
                  makeFilterConvolutionList
                    rows
                    cols
                    (fourierMellinTransform rf af alpha))
               afs)
          rfs
  filters <-
    M.mapM (dftExecuteBatch plan (DFTPlanID DFT2D [rows, cols] [])) filterList
  return filters
makeFourierMellinTransformFilterConvolutionFilter _ _ _ =
  error
    "makeFourierMellinTransformFilterConvolutionFilter: filter parameter type error."
    
{-# INLINE makeFourierMellinTransformFilterConvolutionPI #-}

makeFourierMellinTransformFilterConvolutionPI
  :: DFTPlan
  -> PolarSeparableFilterParams
  -> ConvolutionalFilterType
  -> IO (DFTPlan, [[VS.Vector (Complex Double)]])
makeFourierMellinTransformFilterConvolutionPI plan (FourierMellinTransformParams rows cols rfs afs alpha) filterType = do
  let filterTemp =
        VS.fromList . conjugateFunc filterType $!
        makeFilterConvolutionList
          rows
          cols
          (fourierMellinTransformPI (L.last rfs) (L.last afs) alpha)
      filterList =
        L.map
          (\rf ->
             L.map
               (\af ->
                  VS.fromList . conjugateFunc filterType $!
                  makeFilterConvolutionList
                    rows
                    cols
                    (fourierMellinTransformPI rf af alpha))
               afs)
          rfs
  lock <- getFFTWLock
  (p1, vec) <- dft2dPlan lock plan rows cols filterTemp
  (p2, _) <- idft2dPlan lock p1 rows cols vec
  filters <-
    M.mapM (dftExecuteBatch p2 (DFTPlanID DFT2D [rows, cols] [])) filterList
  return (p2, filters)
makeFourierMellinTransformFilterConvolutionPI _ _ _ =
  error
    "makeFourierMellinTransformFilterConvolutionPI: filter parameter type error."
    
{-# INLINE applyFourierMellinTransformFilterConvolution #-}

applyFourierMellinTransformFilterConvolution
  :: DFTPlan
  -> Int
  -> Int
  -> [[VS.Vector (Complex Double)]]
  -> [VS.Vector (Complex Double)]
  -> IO [[VS.Vector (Complex Double)]]
applyFourierMellinTransformFilterConvolution plan rows cols filters xs = do
  ys <- dftExecuteBatch plan (DFTPlanID DFT2D [rows, cols] []) xs
  fmap (\x -> [x]) .
    dftExecuteBatch plan (DFTPlanID IDFT2D [rows, cols] []) .
    L.concatMap (\x -> L.concatMap (L.map (VS.zipWith (*) x)) filters) $
    ys
