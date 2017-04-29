module CV.Utility.FFT where

import           Data.Array.CArray
import           Data.Array.CArray.Base (mallocForeignPtrArrayAligned)
import           Data.Bits              (complement, (.&.), (.|.))
import           Data.Complex
import           Foreign.ForeignPtr     (withForeignPtr)
import           Foreign.Marshal.Array  (copyArray)
import           Foreign.Ptr
import           Foreign.Storable
import           Math.FFT.Base          hiding (dftG, dftGU, dftN, idftN,
                                         transformCArray, transformCArray')

dftN
  :: (FFTWReal r, Ix i, Shapable i)
  => [Int] -> CArray i (Complex r) -> IO (CArray i (Complex r))
dftN = dftG DFTForward estimate

idftN
  :: (FFTWReal r, Ix i, Shapable i)
  => [Int] -> CArray i (Complex r) -> IO (CArray i (Complex r))
idftN = dftG DFTBackward estimate

dftG
  :: (FFTWReal r, Ix i, Shapable i)
  => Sign -> Flag -> [Int] -> CArray i (Complex r) -> IO (CArray i (Complex r))
dftG s f tdims ain =
  case s of
    DFTForward  -> dftGU s f tdims ain
    DFTBackward -> fmap (unsafeNormalize tdims) (dftGU s f tdims ain)

dftGU
  :: (FFTWReal r, Ix i, Shapable i)
  => Sign -> Flag -> [Int] -> CArray i (Complex r) -> IO (CArray i (Complex r))
dftGU s f tdims ain = transformCArray f ain bds go
  where
    go f' ip op =
      withTSpec tspec $
      \r ds hr hds -> plan_guru_dft r ds hr hds ip op (unSign s) f'
    (bds, tspec) = dftShape CC tdims ain

{-# NOINLINE transformCArray #-}

transformCArray
  :: (Ix i, Storable a, Storable b)
  => Flag
  -> CArray i a
  -> (i, i)
  -> (FFTWFlag -> Ptr a -> Ptr b -> IO Plan)
  -> IO (CArray i b)
transformCArray f a lu planner =
  if f `has` estimate && not (any (f `has`) [patient, exhaustive])
    then go
    else transformCArray' f a lu planner
  where
    go = do
      ofp <- mallocForeignPtrArrayAligned (rangeSize lu)
      withCArray a $
        \ip ->
           withForeignPtr ofp $
           \op -> do
             p <- withLock $ planner (unFlag f) ip op
             execute p
      unsafeForeignPtrToCArray ofp lu

{-# NOINLINE transformCArray' #-}

transformCArray'
  :: (Ix i, Storable a, Storable b)
  => Flag
  -> CArray i a
  -> (i, i)
  -> (FFTWFlag -> Ptr a -> Ptr b -> IO Plan)
  -> IO (CArray i b)
transformCArray' f a lu planner = do
  ofp <- mallocForeignPtrArrayAligned (rangeSize lu)
  wfp <- mallocForeignPtrArrayAligned sz
  withCArray a $
    \ip ->
       withForeignPtr ofp $
       \op ->
          withForeignPtr wfp $
          \wp -> do
            p <- withLock $ planner (unFlag f') wp op
            copyArray wp ip sz
            execute p
  unsafeForeignPtrToCArray ofp lu
  where
    sz = size a
    f' = f .&. complement preserveInput .|. destroyInput
