{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module CV.Utility.FFT.Base (
   module CV.Utility.FFT.Base,
   module CV.Utility.FFT.FFI,
   ) where

import CV.Utility.FFT.FFI

import qualified Foreign.C.Types as C
import Foreign.C.String (withCString, peekCString)

import Foreign.Ptr (nullPtr)
import System.IO.Unsafe (unsafePerformIO)

import Control.Monad (when)
import Control.Exception (finally)
import Control.Concurrent.MVar (MVar, newMVar, withMVar)

import Data.Bits (Bits)
import Data.Typeable ()



-- | This lock must be taken during /planning/ of any transform.  The FFTW
-- library is not thread-safe in the planning phase.  Thankfully, the lock is
-- not needed during the execute phase.
lock :: MVar ()
lock = unsafePerformIO $ newMVar ()
{-# NOINLINE lock #-}

withLock :: IO a -> IO a
withLock = withMVar lock . const

-- | The 'Flag' type is used to influence the kind of plans which are created.
-- To specify multiple flags, use a bitwise '.|.'.
newtype Flag = Flag { unFlag :: FFTWFlag }
    deriving (Eq, Show, Num, Bits)

-- | Default flag.  For most transforms, this is equivalent to setting 'measure'
-- and 'preserveInput'.  The exceptions are complex to real and half-complex to
-- real transforms.
nullFlag :: Flag
nullFlag = Flag 0

--
-- Algorithm restriction flags
--

-- | Allows FFTW to overwrite the input array with arbitrary data; this can
-- sometimes allow more efficient algorithms to be employed.
--
-- Setting this flag implies that two memory allocations will be done, one for
-- work space, and one for the result.  When 'estimate' is not set, we will be
-- doing two memory allocations anyway, so we set this flag as well (since we
-- don't retain the work array anyway).
destroyInput :: Flag
destroyInput = Flag c_destroy_input

-- | 'preserveInput' specifies that an out-of-place transform must not change
-- its input array. This is ordinarily the default, except for complex to real
-- transforms for which 'destroyInput' is the default. In the latter cases,
-- passing 'preserveInput' will attempt to use algorithms that do not destroy
-- the input, at the expense of worse performance; for multi-dimensional complex
-- to real transforms, however, no input-preserving algorithms are implemented
-- so the Haskell bindings will set 'destroyInput' and do a transform with two
-- memory allocations.
preserveInput :: Flag
preserveInput = Flag c_preserve_input

-- | Instruct FFTW not to generate a plan which uses SIMD instructions, even if
-- the memory you are planning with is aligned.  This should only be needed if
-- you are using the guru interface and want to reuse a plan with memory that
-- may be unaligned (i.e. you constructed the 'CArray' with
-- 'unsafeForeignPtrToCArray').
unaligned :: Flag
unaligned = Flag c_unaligned

-- | The header claims that this flag is documented, but in reality, it is not.
-- I don't know what it does and it is here only for completeness.
conserveMemory :: Flag
conserveMemory = Flag c_conserve_memory

--
-- Planning rigor flags
--

-- | 'estimate' specifies that, instead of actual measurements of different
-- algorithms, a simple heuristic is used to pick a (probably sub-optimal) plan
-- quickly. With this flag, the input/output arrays are not overwritten during
-- planning.
--
-- This is the only planner flag for which a single memory allocation is possible.
estimate :: Flag
estimate = Flag c_estimate

-- | 'measure' tells FFTW to find an optimized plan by actually computing
-- several FFTs and measuring their execution time. Depending on your machine,
-- this can take some time (often a few seconds). 'measure' is the default
-- planning option.
measure :: Flag
measure = Flag c_measure

-- | 'patient' is like 'measure', but considers a wider range of algorithms and
-- often produces a "more optimal" plan (especially for large transforms), but
-- at the expense of several times longer planning time (especially for large
-- transforms).
patient :: Flag
patient = Flag c_patient

-- | 'exhaustive' is like 'patient' but considers an even wider range of
-- algorithms, including many that we think are unlikely to be fast, to
-- produce the most optimal plan but with a substantially increased planning
-- time.
exhaustive :: Flag
exhaustive = Flag c_exhaustive


wisdomOnly :: Flag
wisdomOnly = Flag 2 ^ (21 :: Int)



-- | Determine which direction of DFT to execute.
data Sign = DFTForward | DFTBackward
    deriving (Eq,Show)

unSign :: Sign -> FFTWSign
unSign DFTForward = c_forward
unSign DFTBackward = c_backward

-- | Real to Real transform kinds.
data Kind = R2HC | HC2R                             -- half-complex transforms
          | DHT                                     -- discrete Hartley transformm
          | REDFT00 | REDFT10 | REDFT01 | REDFT11   -- discrete cosine transforms
          | RODFT00 | RODFT01 | RODFT10 | RODFT11   -- discrete sine transforms
    deriving (Eq,Show)

unKind :: Kind -> FFTWKind
unKind k = case k of
               R2HC -> c_r2hc
               HC2R -> c_hc2r
               DHT -> c_dht
               REDFT00 -> c_redft00
               REDFT10 -> c_redft10
               REDFT01 -> c_redft01
               REDFT11 -> c_redft11
               RODFT00 -> c_rodft00
               RODFT01 -> c_rodft01
               RODFT10 -> c_rodft10
               RODFT11 -> c_rodft11

-- | Types of transforms.  Used to control 'dftShape'.
data DFT = CC | RC | CR | CRO | RR
    deriving (Eq, Show)

-- | Verify that a plan is valid.  Thows an exception if not.
check :: Plan -> IO ()
check p = when (p == nullPtr) . ioError $ userError "invalid plan"

-- | Confirm that the plan is valid, then execute the transform.
execute :: Plan -> IO ()
execute p = check p >> c_execute p

-- | Queries the FFTW cache.  The 'String' can be written to a file so the
-- wisdom can be reused on a subsequent run.
exportWisdomString :: IO String
exportWisdomString = do
    pc <- c_export_wisdom_string
    peekCString pc `finally` c_free pc

-- | Add wisdom to the FFTW cache.  Returns 'True' if it is successful.
importWisdomString :: String -> IO Bool
importWisdomString str =
    (==1) <$> withCString str c_import_wisdom_string

-- | Tries to import wisdom from a global source, typically @/etc/fftw/wisdom@.
-- Returns 'True' if it was successful.
importWisdomSystem :: IO Bool
importWisdomSystem = (==1) <$> c_import_wisdom_system
