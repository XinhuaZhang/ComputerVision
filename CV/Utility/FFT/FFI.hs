{-# LINE 1 "FFI.hsc" #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LINE 2 "FFI.hsc" #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module CV.Utility.FFT.FFI where

import qualified Foreign.C.Types as C
import Foreign.C.String (CString)
import Foreign.Ptr (Ptr)
import Foreign.Storable
          (Storable, sizeOf, alignment, peek, poke, peekByteOff, pokeByteOff)
import Foreign.Storable.Complex ()

import Data.Complex (Complex)
import Data.Generics (Data, Typeable)
import Data.Typeable ()



{-# LINE 18 "FFI.hsc" #-}


type FFTWFlag = C.CUInt

c_measure          :: FFTWFlag
c_measure          =  0
c_destroy_input    :: FFTWFlag
c_destroy_input    =  1
c_unaligned        :: FFTWFlag
c_unaligned        =  2
c_conserve_memory  :: FFTWFlag
c_conserve_memory  =  4
c_exhaustive       :: FFTWFlag
c_exhaustive       =  8
c_preserve_input   :: FFTWFlag
c_preserve_input   =  16
c_patient          :: FFTWFlag
c_patient          =  32
c_estimate         :: FFTWFlag
c_estimate         =  64

{-# LINE 32 "FFI.hsc" #-}


type FFTWSign = C.CInt

c_forward  :: FFTWSign
c_forward  =  (-1)
c_backward  :: FFTWSign
c_backward  =  1

{-# LINE 40 "FFI.hsc" #-}


type FFTWKind = C.CInt

c_r2hc     :: FFTWKind
c_r2hc     =  0
c_hc2r     :: FFTWKind
c_hc2r     =  1
c_dht      :: FFTWKind
c_dht      =  2
c_redft00  :: FFTWKind
c_redft00  =  3
c_redft10  :: FFTWKind
c_redft10  =  5
c_redft01  :: FFTWKind
c_redft01  =  4
c_redft11  :: FFTWKind
c_redft11  =  6
c_rodft00  :: FFTWKind
c_rodft00  =  7
c_rodft10  :: FFTWKind
c_rodft10  =  9
c_rodft01  :: FFTWKind
c_rodft01  =  8
c_rodft11  :: FFTWKind
c_rodft11  =  10

{-# LINE 57 "FFI.hsc" #-}


-- | Corresponds to the @fftw_iodim@ structure.  It completely describes the
-- layout of each dimension, before and after the transform.
data IODim = IODim { nIODim :: Int  -- ^ Logical size of dimension
                   , isIODim :: Int -- ^ Stride along dimension in input array
                   , osIODim :: Int -- ^ Stride along dimension in output array
                   }
    deriving (Eq, Show, Data, Typeable)

instance Storable IODim where
    sizeOf _ = (12)
{-# LINE 69 "FFI.hsc" #-}
    alignment _ = alignment (undefined :: C.CInt)
    peek p = do
        n' <- (\hsc_ptr -> peekByteOff hsc_ptr 0) p
{-# LINE 72 "FFI.hsc" #-}
        is' <- (\hsc_ptr -> peekByteOff hsc_ptr 4) p
{-# LINE 73 "FFI.hsc" #-}
        os' <- (\hsc_ptr -> peekByteOff hsc_ptr 8) p
{-# LINE 74 "FFI.hsc" #-}
        return (IODim n' is' os')
    poke p (IODim n' is' os') = do
        (\hsc_ptr -> pokeByteOff hsc_ptr 0) p n'
{-# LINE 77 "FFI.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 4) p is'
{-# LINE 78 "FFI.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 8) p os'
{-# LINE 79 "FFI.hsc" #-}


-- | A plan is an opaque foreign object.
type Plan = Ptr FFTWPlan

type FFTWPlan = ()

-- We use "safe" calls for anything which could take a while so that it won't block
-- other Haskell threads.

-- | Plan a complex to complex transform using the guru interface.
foreign import ccall safe "fftw3.h fftwf_plan_guru_dft" cf_plan_guru_dft
    :: C.CInt -> Ptr IODim -> C.CInt -> Ptr IODim -> Ptr (Complex Float)
    -> Ptr (Complex Float) -> FFTWSign -> FFTWFlag -> IO Plan

-- | Plan a real to complex transform using the guru interface.
foreign import ccall safe "fftw3.h fftwf_plan_guru_dft_r2c" cf_plan_guru_dft_r2c
    :: C.CInt -> Ptr IODim -> C.CInt -> Ptr IODim -> Ptr Float
    -> Ptr (Complex Float) -> FFTWFlag -> IO Plan

-- | Plan a complex to real transform using the guru interface.
foreign import ccall safe "fftw3.h fftwf_plan_guru_dft_c2r" cf_plan_guru_dft_c2r
    :: C.CInt -> Ptr IODim -> C.CInt -> Ptr IODim -> Ptr (Complex Float)
    -> Ptr Float -> FFTWFlag -> IO Plan

-- | Plan a real to real transform using the guru interface.
foreign import ccall safe "fftw3.h fftwf_plan_guru_r2r" cf_plan_guru_r2r
    :: C.CInt -> Ptr IODim -> C.CInt -> Ptr IODim -> Ptr Float
    -> Ptr Float -> Ptr FFTWKind -> FFTWFlag -> IO Plan


-- | Plan a complex to complex transform using the guru interface.
foreign import ccall safe "fftw3.h fftw_plan_guru_dft" c_plan_guru_dft
    :: C.CInt -> Ptr IODim -> C.CInt -> Ptr IODim -> Ptr (Complex Double)
    -> Ptr (Complex Double) -> FFTWSign -> FFTWFlag -> IO Plan

-- | Plan a real to complex transform using the guru interface.
foreign import ccall safe "fftw3.h fftw_plan_guru_dft_r2c" c_plan_guru_dft_r2c
    :: C.CInt -> Ptr IODim -> C.CInt -> Ptr IODim -> Ptr Double
    -> Ptr (Complex Double) -> FFTWFlag -> IO Plan

-- | Plan a complex to real transform using the guru interface.
foreign import ccall safe "fftw3.h fftw_plan_guru_dft_c2r" c_plan_guru_dft_c2r
    :: C.CInt -> Ptr IODim -> C.CInt -> Ptr IODim -> Ptr (Complex Double)
    -> Ptr Double -> FFTWFlag -> IO Plan

-- | Plan a real to real transform using the guru interface.
foreign import ccall safe "fftw3.h fftw_plan_guru_r2r" c_plan_guru_r2r
    :: C.CInt -> Ptr IODim -> C.CInt -> Ptr IODim -> Ptr Double
    -> Ptr Double -> Ptr FFTWKind -> FFTWFlag -> IO Plan

-- | Simple plan execution
foreign import ccall safe "fftw3.h fftw_execute" c_execute
    :: Plan -> IO ()

-- Execute a plan on different memory than the plan was created for.
-- Alignment /must/ be the same.  If we parallelize a transform of
-- multi-dimensional data by making separate calls within an un-transformed
-- dimension, it is possible that the alignment constraint would not be
-- fulfilled.  However, this only poses a problem for real transforms with odd
-- transform dimension.
foreign import ccall safe "fftw3.h fftw_execute_dft" c_execute_dft
    :: Plan -> Ptr (Complex Double) -> Ptr (Complex Double) -> IO ()
foreign import ccall safe "fftw3.h fftw_execute_dft_r2c" c_execute_dft_r2c
    :: Plan -> Ptr Double -> Ptr (Complex Double) -> IO ()
foreign import ccall safe "fftw3.h fftw_execute_dft_c2r" c_execute_dft_c2r
    :: Plan -> Ptr (Complex Double) -> Ptr Double -> IO ()
foreign import ccall safe "fftw3.h fftw_execute_r2r" c_execute_r2r
    :: Plan -> Ptr Double -> Ptr Double -> IO ()

foreign import ccall safe "fftw3.h fftw_export_wisdom_to_string"
        c_export_wisdom_string :: IO CString

foreign import ccall safe "fftw3.h fftw_import_wisdom_from_string"
        c_import_wisdom_string :: CString -> IO C.CInt

foreign import ccall safe "fftw3.h fftw_import_system_wisdom"
        c_import_wisdom_system :: IO C.CInt

-- | Frees memory allocated by 'fftw_malloc'.  Currently, we only need this to
-- free the wisdom string.
foreign import ccall safe "fftw3.h fftw_free" c_free :: Ptr a -> IO ()


foreign import ccall safe "fftw3.h fftw_destroy_plan" c_destroy_plan ::  Plan -> IO ()

foreign import ccall safe "fftw3.h fftw_cleanup" c_cleanup :: IO ()

foreign import ccall safe "fftw3.h fftw_plan_dft_2d" c_plan_dft_2d :: C.CInt -> C.CInt -> Ptr (Complex Double) -> Ptr (Complex Double) -> C.CInt -> FFTWFlag -> IO Plan
