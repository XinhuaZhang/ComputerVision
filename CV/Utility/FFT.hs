module CV.Utility.FFT
  ( FFTWWisdom(..)
  , FFTW(..)
  , initializefftw
  , dft2d
  , idft2d
  , dft1d
  , idft1d
  , dftr2c
  , exportWisdomString
  , importWisdomString
  , generateWisdom
  ) where

import           Control.Concurrent.MVar
import           CV.Utility.FFT.Base          (Flag (..), Sign (..), estimate,
                                               exhaustive, exportWisdomString,
                                               importWisdomString, measure,
                                               patient, unFlag, unSign,
                                               wisdomOnly)
import           CV.Utility.FFT.FFI
import           Data.Bits                    ((.|.))
import           Data.Complex
import           Data.List                    as L
import           Data.Vector.Storable         as VS
import           Data.Vector.Storable.Mutable as VSM
import           Foreign.C.Types

data FFTWWisdom
  = FFTWWisdomPath String
  | FFTWWisdomNull

data FFTW = FFTW
  { fftwLock       :: !(MVar ())
  , fftwWisdomFlag :: !Bool
  }

initializefftw :: FFTWWisdom -> IO FFTW
initializefftw FFTWWisdomNull = do
  lock <- newMVar ()
  return $! FFTW lock False
initializefftw (FFTWWisdomPath path) = do
  str <- readFile path
  flag <- importWisdomString str
  if flag
    then do
      lock <- newMVar ()
      return $ FFTW lock True
    else error $ "initializefftw: importWisdomString (" L.++ str L.++ ")"

generateWisdom :: FFTW
               -> FilePath
               -> Int
               -> Int
               -> VS.Vector (Complex Double)
               -> IO ()
generateWisdom fftw path rows cols vec = do
  _x <- dft2d fftw rows cols vec
  _x <- idft2d fftw rows cols vec
  wisdom <- exportWisdomString
  writeFile path wisdom

{-# INLINE dft2d #-}

dft2d
  :: FFTW
  -> Int
  -> Int
  -> VS.Vector (Complex Double)
  -> IO (VS.Vector (Complex Double))
dft2d (FFTW lock' True) rows cols vec =
  dft2dG lock' rows cols vec DFTForward (estimate .|. wisdomOnly)
dft2d (FFTW lock' False) rows cols vec = dft2dG lock' rows cols vec DFTForward measure

{-# INLINE idft2d #-}

idft2d
  :: FFTW
  -> Int
  -> Int
  -> VS.Vector (Complex Double)
  -> IO (VS.Vector (Complex Double))
idft2d (FFTW lock' True) rows cols vec =
  VS.map (/ (fromIntegral $ rows * cols)) <$>
  dft2dG lock' rows cols vec DFTBackward (estimate .|. wisdomOnly)
idft2d (FFTW lock' False) rows cols vec =
  VS.map (/ (fromIntegral $ rows * cols)) <$>
  dft2dG lock' rows cols vec DFTBackward measure

{-# INLINE dft2dG #-}

dft2dG
  :: MVar ()
  -> Int
  -> Int
  -> VS.Vector (Complex Double)
  -> Sign
  -> Flag
  -> IO (VS.Vector (Complex Double))
dft2dG lock' rows cols vec sign flag = do
  v <- VSM.new (rows * cols)
  x <- takeMVar lock'
  VS.unsafeWith vec $
    \ip ->
       VSM.unsafeWith v $
       \op -> do
         p <-
           c_plan_dft_2d
             (fromIntegral rows)
             (fromIntegral cols)
             ip
             op
             (unSign sign)
             (unFlag flag)
         c_execute p
         c_destroy_plan p
  putMVar lock' x
  VS.freeze v

{-# INLINE dftr2c #-}

dftr2c :: FFTW -> VS.Vector CDouble -> IO (VS.Vector (Complex Double))
dftr2c (FFTW lock' _) vec = do
  v <- VSM.new (div n 2 + 1)
  x <- takeMVar lock'
  VS.unsafeWith vec $
    \ip ->
       VSM.unsafeWith v $
       \op -> do
         p <- c_plan_dft_r2c_1d (fromIntegral n) ip op (unFlag estimate)
         c_execute p
         c_destroy_plan p
  putMVar lock' x
  VS.freeze v
  where
    n = VS.length vec

{-# INLINE dft1d #-}

dft1d :: FFTW -> VS.Vector (Complex Double) -> IO (VS.Vector (Complex Double))
dft1d (FFTW lock' _) vec = do
  v <- VSM.new n
  x <- takeMVar lock'
  VS.unsafeWith vec $
    \ip ->
       VSM.unsafeWith v $
       \op -> do
         p <-
           c_plan_dft_1d
             (fromIntegral n)
             ip
             op
             (unSign DFTForward)
             (unFlag estimate)
         c_execute p
         c_destroy_plan p
  putMVar lock' x
  VS.freeze v
  where
    n = VS.length vec
    

{-# INLINE idft1d #-}

idft1d :: FFTW -> VS.Vector (Complex Double) -> IO (VS.Vector (Complex Double))
idft1d (FFTW lock' _) vec = do
  v <- VSM.new n
  x <- takeMVar lock'
  VS.unsafeWith vec $
    \ip ->
       VSM.unsafeWith v $
       \op -> do
         p <-
           c_plan_dft_1d
             (fromIntegral n)
             ip
             op
             (unSign DFTBackward)
             (unFlag estimate)
         c_execute p
         c_destroy_plan p
  putMVar lock' x
  VS.freeze v
  where
    n = VS.length vec
