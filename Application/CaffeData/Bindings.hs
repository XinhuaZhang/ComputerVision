{-# LINE 1 "Bindings.hsc" #-}

{-# LINE 2 "Bindings.hsc" #-}

{-# LINE 3 "Bindings.hsc" #-}

module Application.CaffeData.Bindings  where
import Foreign.Ptr (Ptr,FunPtr,plusPtr)
import Foreign.Ptr (wordPtrToPtr,castPtrToFunPtr)
import Foreign.Storable
import Foreign.C.Types
import Foreign.C.String (CString,CStringLen,CWString,CWStringLen)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (peekArray,pokeArray)
import Data.Int
import Data.Word

{-# LINE 6 "Bindings.hsc" #-}
foreign import ccall "openDatabase" c'openDatabase
  :: CString -> CString -> IO ()
foreign import ccall "&openDatabase" p'openDatabase
  :: FunPtr (CString -> CString -> IO ())

{-# LINE 7 "Bindings.hsc" #-}
foreign import ccall "closeDatabase" c'closeDatabase
  :: IO ()
foreign import ccall "&closeDatabase" p'closeDatabase
  :: FunPtr (IO ())

{-# LINE 8 "Bindings.hsc" #-}
foreign import ccall "saveData" c'saveData
  :: CInt -> CInt -> CInt -> CInt -> CInt -> Ptr (Ptr CUChar) -> Ptr CInt -> IO ()
foreign import ccall "&saveData" p'saveData
  :: FunPtr (CInt -> CInt -> CInt -> CInt -> CInt -> Ptr (Ptr CUChar) -> Ptr CInt -> IO ())

{-# LINE 9 "Bindings.hsc" #-}
-- #ccall saveData , CInt ->  CInt ->  CInt -> CInt -> CInt -> Ptr (Ptr CFloat) -> Ptr CInt -> IO ()
