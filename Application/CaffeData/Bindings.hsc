#include "bindings.dsl.h"
#include "caffeData.h"        

module Application.CaffeData.Bindings  where
#strict_import        
#ccall openDatabase , CString -> CString -> CInt -> IO ()
#ccall closeDatabase , IO ()
#ccall saveData , CInt -> CInt ->  CInt ->  CInt -> CInt -> CInt -> Ptr (Ptr CUChar) -> Ptr CInt -> IO ()
-- #ccall saveData , CInt ->  CInt ->  CInt -> CInt -> CInt -> Ptr (Ptr CFloat) -> Ptr CInt -> IO ()
