module CV.Utility.FFT where

import           Control.Concurrent.MVar 
import           Data.Array.CArray
import           Data.Array.CArray.Base  (mallocForeignPtrArrayAligned)
import           Data.Bits               (complement, (.&.), (.|.))
import           Data.Complex
import           Foreign.ForeignPtr
import           Foreign.Marshal.Array   (copyArray)
import           Foreign.Ptr
import           Foreign.Storable
import           Math.FFT.Base           hiding (dftG, dftGU, dftN, idftN,
                                          transformCArray, transformCArray')

{-# INLINE dftN #-}

dftN
  :: (FFTWReal r, Ix i, Shapable i)
  => MVar ()
  -> [Int]
  -> CArray i (Complex r)
  -> IO (CArray i (Complex r))
dftN = dftG DFTForward estimate

{-# INLINE idftN #-}

idftN
  :: (FFTWReal r, Ix i, Shapable i)
  => MVar ()
  -> [Int]
  -> CArray i (Complex r)
  -> IO (CArray i (Complex r))
idftN = dftG DFTBackward estimate

{-# INLINE dftG #-}

dftG
  :: (FFTWReal r, Ix i, Shapable i)
  => Sign
  -> Flag
  -> MVar ()
  -> [Int]
  -> CArray i (Complex r)
  -> IO (CArray i (Complex r))
dftG s f m tdims ain =
  case s of
    DFTForward  -> dftGU m s f tdims ain
    DFTBackward -> fmap (unsafeNormalize tdims) (dftGU m s f tdims ain)

{-# INLINE dftGU #-}

dftGU
  :: (FFTWReal r, Ix i, Shapable i)
  => MVar ()
  -> Sign
  -> Flag
  -> [Int]
  -> CArray i (Complex r)
  -> IO (CArray i (Complex r))
dftGU m s f tdims ain = transformCArray m f ain bds go
  where
    go f' ip op =
      withTSpec tspec $
      \r ds hr hds -> plan_guru_dft r ds hr hds ip op (unSign s) f'
    (bds, tspec) = dftShape CC tdims ain

{-# INLINE transformCArray #-}

transformCArray
  :: (Ix i, Storable a, Storable b)
  => MVar ()
  -> Flag
  -> CArray i a
  -> (i, i)
  -> (FFTWFlag -> Ptr a -> Ptr b -> IO Plan)
  -> IO (CArray i b)
transformCArray m f a lu planner =
  if f `has` estimate && not (any (f `has`) [patient, exhaustive])
    then go
    else transformCArray' m f a lu planner
  where
    go = do
      x <- takeMVar m
      ofp <- mallocForeignPtrArrayAligned (rangeSize lu)
      withCArray a $
        \ip ->
           withForeignPtr ofp $
           \op -> do
             p <- planner (unFlag f) ip op
             putMVar m x
             execute p
      unsafeForeignPtrToCArray ofp lu

{-# INLINE transformCArray' #-}

transformCArray'
  :: (Ix i, Storable a, Storable b)
  => MVar ()
  -> Flag
  -> CArray i a
  -> (i, i)
  -> (FFTWFlag -> Ptr a -> Ptr b -> IO Plan)
  -> IO (CArray i b)
transformCArray' m f a lu planner = do
  x <- takeMVar m
  ofp <- mallocForeignPtrArrayAligned (rangeSize lu)
  wfp <- mallocForeignPtrArrayAligned sz
  withCArray a $
    \ip ->
       withForeignPtr ofp $
       \op ->
          withForeignPtr wfp $
          \wp -> do
            p <- planner (unFlag f') wp op
            copyArray wp ip sz
            execute p
  putMVar m x
  unsafeForeignPtrToCArray ofp lu
  where
    sz = size a
    f' = f .&. complement preserveInput .|. destroyInput
