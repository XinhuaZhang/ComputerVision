#include <hdf5.h>
#include <hdf5_hl.h>
#include <bindings.dsl.h>

module Application.CaffeData.HDF5Bindings where
#strict_import

#integral_t hsize_t
#integral_t size_t
#integral_t herr_t
#integral_t hid_t

#integral_t H5T_class_t
#num H5T_NO_CLASS
#num H5T_INTEGER
#num H5T_FLOAT
#num H5T_TIME
#num H5T_STRING
#num H5T_BITFIELD
#num H5T_OPAQUE
#num H5T_COMPOUND
#num H5T_REFERENCE
#num H5T_ENUM
#num H5T_VLEN
#num H5T_ARRAY
#num H5T_NCLASSES

#ccall H5LTget_dataset_ndims , <hid_t> -> CString -> Ptr CInt -> IO <herr_t>
#ccall H5LTget_dataset_info , <hid_t> -> CString -> Ptr <hsize_t> -> Ptr <H5T_class_t> -> Ptr <size_t> -> IO <herr_t>

#ccall H5LTmake_dataset_float , <hid_t> -> CString -> CInt -> Ptr <hsize_t> -> Ptr CFloat -> IO <herr_t>
#ccall H5LTmake_dataset_double , <hid_t> -> CString -> CInt -> Ptr <hsize_t> -> Ptr CDouble -> IO <herr_t>
#ccall H5LTread_dataset_float , <hid_t> -> CString -> Ptr CFloat -> IO <herr_t>
#ccall H5LTread_dataset_double , <hid_t> -> CString -> Ptr CDouble -> IO <herr_t>

#ccall H5Fopen , Ptr CChar -> CUInt -> <hid_t> -> IO <hid_t>
#ccall H5Fcreate , Ptr CChar -> CUInt -> <hid_t> -> <hid_t> -> IO <hid_t>
#ccall H5Fclose , <hid_t> -> IO <herr_t>

h5f_acc_trunc :: CUInt
h5f_acc_trunc = #const H5F_ACC_TRUNC

h5f_acc_excl :: CUInt
h5f_acc_excl =  #const H5F_ACC_EXCL

h5p_default :: CInt
h5p_default = #const H5P_DEFAULT

h5f_acc_rdwr :: CUInt
h5f_acc_rdwr = #const H5F_ACC_RDWR

h5f_acc_rdonly :: CUInt
h5f_acc_rdonly = #const H5F_ACC_RDONLY
