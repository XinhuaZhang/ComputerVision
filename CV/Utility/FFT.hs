module CV.Utility.FFT
  ( FFTWWisdom(..)
  , FFTW(..)
  , initializefftw
  , dft2d
  , idft2d
  , dft1d
  , idft1d
  , dftr2c
  , dft1dG
  , idft1dG
  , dftr2c2d
  , idftr2c2d
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
import           Foreign.Marshal.Array

data FFTWWisdom
  = FFTWWisdomPath String
  | FFTWWisdomNull
  deriving (Show)

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
generateWisdom fftw@(FFTW lock' False) path rows cols vec = do
  let vec' = VS.fromList . VS.toList $ vec
  _x <- dft2d fftw rows cols vec'
  vec'' <- dft2d (FFTW lock' True) rows cols vec
  _x <- idft2d fftw rows cols vec''
  wisdom <- exportWisdomString
  writeFile path wisdom
generateWisdom _ _ _ _ _ = error "generateWisdom: Flag is True."

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



-- This is a generalied 1d dft, the 1 dimension can be many dimensions
-- which are ascending ordered and continued. For example, given a N
-- dimension array, the generalized 1d dft dimension is
-- either [0,1..M] or [M,M+1.. N-1], where 0 <= M and M <= N-1
-- the dimension corresponding to the largest index spins the fastest. 

{-# INLINE dft1dGGeneric #-}

dft1dGGeneric
  :: MVar ()
  -> [Int]
  -> [Int]
  -> VS.Vector (Complex Double)
  -> Sign
  -> Flag
  -> IO (VS.Vector (Complex Double))
dft1dGGeneric lock' dims dftIndex vec sign flag
  | L.and (L.zipWith (\a b -> a + 1 == b) dftIndex . L.tail $ dftIndex) &&
      (not . L.null $ dftIndex) &&
      (L.head dftIndex == 0 || L.last dftIndex == rank - 1) = do
    v <- VSM.new . L.product $ dims
    x <- takeMVar lock'
    VS.unsafeWith vec $
      \ip ->
         VSM.unsafeWith v $
         \op ->
            withArray (L.map fromIntegral dftDims) $
            \n -> do
              let totalNum = L.product dims
                  dftNum = L.product dftDims
                  stride =
                    if L.last dftIndex == rank - 1
                      then 1
                      else L.product . L.drop (1 + L.last dftIndex) $ dims
                  dist =
                    if L.last dftIndex == rank - 1
                      then dftNum
                      else 1
              p <-
                c_plan_many_dft
                  (fromIntegral dftRank)
                  n
                  (fromIntegral $ div totalNum dftNum)
                  ip
                  n
                  (fromIntegral stride)
                  (fromIntegral dist)
                  op
                  n
                  (fromIntegral stride)
                  (fromIntegral dist)
                  (unSign sign)
                  (unFlag flag)
              c_execute p
              c_destroy_plan p
    putMVar lock' x
    VS.freeze v
  | otherwise =
    error
      "dft1dG: dimension list doesn't satisify the restriction of generalized 1d dft."
  where
    rank = L.length dims
    dftRank = L.length dftIndex
    dftDims =
      L.take (L.last dftIndex - L.head dftIndex + 1) . L.drop (L.head dftIndex) $
      dims

dft1dG
  :: FFTW
  -> [Int]
  -> [Int]
  -> VS.Vector (Complex Double)
  -> IO (VS.Vector (Complex Double))
dft1dG (FFTW lock' True) dims dftIndex vec =
  dft1dGGeneric lock' dims dftIndex vec DFTForward estimate --(estimate .|. wisdomOnly)
dft1dG (FFTW lock' False) dims dftIndex vec =
  dft1dGGeneric lock' dims dftIndex vec DFTForward estimate -- measure

idft1dG
  :: FFTW
  -> [Int]
  -> [Int]
  -> VS.Vector (Complex Double)
  -> IO (VS.Vector (Complex Double))
idft1dG (FFTW lock' True) dims dftIndex vec =
  dft1dGGeneric lock' dims dftIndex vec DFTBackward estimate --(estimate .|. wisdomOnly)
idft1dG (FFTW lock' False) dims dftIndex vec =
  dft1dGGeneric lock' dims dftIndex vec DFTBackward estimate -- measure



{-# INLINE dftr2c2d #-}

dftr2c2d
  :: FFTW
  -> Int
  -> Int
  -> VS.Vector (Complex Double)
  -> IO (VS.Vector (Complex Double))
dftr2c2d (FFTW lock' True) rows cols vec =
  dftr2c2dG lock' rows cols vec DFTForward (estimate .|. wisdomOnly)
dftr2c2d (FFTW lock' False) rows cols vec = dftr2c2dG lock' rows cols vec DFTForward measure

{-# INLINE idftr2c2d #-}

idftr2c2d
  :: FFTW
  -> Int
  -> Int
  -> VS.Vector (Complex Double)
  -> IO (VS.Vector (Complex Double))
idftr2c2d (FFTW lock' True) rows cols vec =
  VS.map (/ (fromIntegral $ rows * cols)) <$>
  dftr2c2dG lock' rows cols vec DFTBackward (estimate .|. wisdomOnly)
idftr2c2d (FFTW lock' False) rows cols vec =
  VS.map (/ (fromIntegral $ rows * cols)) <$>
  dftr2c2dG lock' rows cols vec DFTBackward measure

{-# INLINE dftr2c2dG #-}

dftr2c2dG
  :: MVar ()
  -> Int
  -> Int
  -> VS.Vector (Complex Double)
  -> Sign
  -> Flag
  -> IO (VS.Vector (Complex Double))
dftr2c2dG lock' rows cols vec sign flag = do
  v <- VSM.new (rows * (div cols 2 + 1))
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
