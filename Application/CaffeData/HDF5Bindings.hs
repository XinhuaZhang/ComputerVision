{-# LINE 1 "HDF5Bindings.hsc" #-}

{-# LINE 2 "HDF5Bindings.hsc" #-}

{-# LINE 3 "HDF5Bindings.hsc" #-}

{-# LINE 4 "HDF5Bindings.hsc" #-}

module Application.CaffeData.HDF5Bindings where
import Foreign.Ptr (Ptr,FunPtr,plusPtr)
import Foreign.Ptr (wordPtrToPtr,castPtrToFunPtr)
import Foreign.Storable
import Foreign.C.Types
import Foreign.C.String (CString,CStringLen,CWString,CWStringLen)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (peekArray,pokeArray)
import Data.Int
import Data.Word

{-# LINE 7 "HDF5Bindings.hsc" #-}

type C'hsize_t = Word64

{-# LINE 9 "HDF5Bindings.hsc" #-}
type C'size_t = Word64

{-# LINE 10 "HDF5Bindings.hsc" #-}
type C'herr_t = CInt

{-# LINE 11 "HDF5Bindings.hsc" #-}
type C'hid_t = CInt

{-# LINE 12 "HDF5Bindings.hsc" #-}

type C'H5T_class_t = CInt

{-# LINE 14 "HDF5Bindings.hsc" #-}
c'H5T_NO_CLASS = -1
c'H5T_NO_CLASS :: (Num a) => a

{-# LINE 15 "HDF5Bindings.hsc" #-}
c'H5T_INTEGER = 0
c'H5T_INTEGER :: (Num a) => a

{-# LINE 16 "HDF5Bindings.hsc" #-}
c'H5T_FLOAT = 1
c'H5T_FLOAT :: (Num a) => a

{-# LINE 17 "HDF5Bindings.hsc" #-}
c'H5T_TIME = 2
c'H5T_TIME :: (Num a) => a

{-# LINE 18 "HDF5Bindings.hsc" #-}
c'H5T_STRING = 3
c'H5T_STRING :: (Num a) => a

{-# LINE 19 "HDF5Bindings.hsc" #-}
c'H5T_BITFIELD = 4
c'H5T_BITFIELD :: (Num a) => a

{-# LINE 20 "HDF5Bindings.hsc" #-}
c'H5T_OPAQUE = 5
c'H5T_OPAQUE :: (Num a) => a

{-# LINE 21 "HDF5Bindings.hsc" #-}
c'H5T_COMPOUND = 6
c'H5T_COMPOUND :: (Num a) => a

{-# LINE 22 "HDF5Bindings.hsc" #-}
c'H5T_REFERENCE = 7
c'H5T_REFERENCE :: (Num a) => a

{-# LINE 23 "HDF5Bindings.hsc" #-}
c'H5T_ENUM = 8
c'H5T_ENUM :: (Num a) => a

{-# LINE 24 "HDF5Bindings.hsc" #-}
c'H5T_VLEN = 9
c'H5T_VLEN :: (Num a) => a

{-# LINE 25 "HDF5Bindings.hsc" #-}
c'H5T_ARRAY = 10
c'H5T_ARRAY :: (Num a) => a

{-# LINE 26 "HDF5Bindings.hsc" #-}
c'H5T_NCLASSES = 11
c'H5T_NCLASSES :: (Num a) => a

{-# LINE 27 "HDF5Bindings.hsc" #-}

foreign import ccall "H5LTget_dataset_ndims" c'H5LTget_dataset_ndims
  :: C'hid_t -> CString -> Ptr CInt -> IO C'herr_t
foreign import ccall "&H5LTget_dataset_ndims" p'H5LTget_dataset_ndims
  :: FunPtr (C'hid_t -> CString -> Ptr CInt -> IO C'herr_t)

{-# LINE 29 "HDF5Bindings.hsc" #-}
foreign import ccall "H5LTget_dataset_info" c'H5LTget_dataset_info
  :: C'hid_t -> CString -> Ptr C'hsize_t -> Ptr C'H5T_class_t -> Ptr C'size_t -> IO C'herr_t
foreign import ccall "&H5LTget_dataset_info" p'H5LTget_dataset_info
  :: FunPtr (C'hid_t -> CString -> Ptr C'hsize_t -> Ptr C'H5T_class_t -> Ptr C'size_t -> IO C'herr_t)

{-# LINE 30 "HDF5Bindings.hsc" #-}

foreign import ccall "H5LTmake_dataset_float" c'H5LTmake_dataset_float
  :: C'hid_t -> CString -> CInt -> Ptr C'hsize_t -> Ptr CFloat -> IO C'herr_t
foreign import ccall "&H5LTmake_dataset_float" p'H5LTmake_dataset_float
  :: FunPtr (C'hid_t -> CString -> CInt -> Ptr C'hsize_t -> Ptr CFloat -> IO C'herr_t)

{-# LINE 32 "HDF5Bindings.hsc" #-}
foreign import ccall "H5LTmake_dataset_double" c'H5LTmake_dataset_double
  :: C'hid_t -> CString -> CInt -> Ptr C'hsize_t -> Ptr CDouble -> IO C'herr_t
foreign import ccall "&H5LTmake_dataset_double" p'H5LTmake_dataset_double
  :: FunPtr (C'hid_t -> CString -> CInt -> Ptr C'hsize_t -> Ptr CDouble -> IO C'herr_t)

{-# LINE 33 "HDF5Bindings.hsc" #-}
foreign import ccall "H5LTread_dataset_float" c'H5LTread_dataset_float
  :: C'hid_t -> CString -> Ptr CFloat -> IO C'herr_t
foreign import ccall "&H5LTread_dataset_float" p'H5LTread_dataset_float
  :: FunPtr (C'hid_t -> CString -> Ptr CFloat -> IO C'herr_t)

{-# LINE 34 "HDF5Bindings.hsc" #-}
foreign import ccall "H5LTread_dataset_double" c'H5LTread_dataset_double
  :: C'hid_t -> CString -> Ptr CDouble -> IO C'herr_t
foreign import ccall "&H5LTread_dataset_double" p'H5LTread_dataset_double
  :: FunPtr (C'hid_t -> CString -> Ptr CDouble -> IO C'herr_t)

{-# LINE 35 "HDF5Bindings.hsc" #-}

foreign import ccall "H5Fopen" c'H5Fopen
  :: Ptr CChar -> CUInt -> C'hid_t -> IO C'hid_t
foreign import ccall "&H5Fopen" p'H5Fopen
  :: FunPtr (Ptr CChar -> CUInt -> C'hid_t -> IO C'hid_t)

{-# LINE 37 "HDF5Bindings.hsc" #-}
foreign import ccall "H5Fcreate" c'H5Fcreate
  :: Ptr CChar -> CUInt -> C'hid_t -> C'hid_t -> IO C'hid_t
foreign import ccall "&H5Fcreate" p'H5Fcreate
  :: FunPtr (Ptr CChar -> CUInt -> C'hid_t -> C'hid_t -> IO C'hid_t)

{-# LINE 38 "HDF5Bindings.hsc" #-}
foreign import ccall "H5Fclose" c'H5Fclose
  :: C'hid_t -> IO C'herr_t
foreign import ccall "&H5Fclose" p'H5Fclose
  :: FunPtr (C'hid_t -> IO C'herr_t)

{-# LINE 39 "HDF5Bindings.hsc" #-}

h5f_acc_trunc :: CUInt
h5f_acc_trunc = 2
{-# LINE 42 "HDF5Bindings.hsc" #-}

h5f_acc_excl :: CUInt
h5f_acc_excl =  4
{-# LINE 45 "HDF5Bindings.hsc" #-}

h5p_default :: CInt
h5p_default = 0
{-# LINE 48 "HDF5Bindings.hsc" #-}

h5f_acc_rdwr :: CUInt
h5f_acc_rdwr = 1
{-# LINE 51 "HDF5Bindings.hsc" #-}

h5f_acc_rdonly :: CUInt
h5f_acc_rdonly = 0
{-# LINE 54 "HDF5Bindings.hsc" #-}
