#include "bindings.dsl.h"
#include "svm.h"
         
module Classifier.LibSVM.Bindings where         
#strict_import

-- svm_node
#starttype struct svm_node
#field index , CInt
#field value , CDouble
#stoptype

-- svm_problme
#starttype struct svm_problem
#field l, CInt
#field y, Ptr CDouble
#field x, Ptr (Ptr <svm_node>)
#stoptype

-- svm_type
#num C_SVC
#num NU_SVC
#num ONE_CLASS
#num EPSILON_SVR
#num NU_SVR

--kernel_type 
#num LINEAR
#num POLY
#num RBF
#num SIGMOID
#num PRECOMPUTED

-- svm_parameter
#starttype struct svm_parameter
#field svm_type , CInt
#field kernel_type , CInt
#field degree , CInt
#field gamma , CDouble
#field coef0 , CDouble
#field cache_size , CDouble 
#field eps , CDouble
#field C , CDouble	
#field nr_weight , CInt
#field weight_label , Ptr CInt	
#field weight , Ptr CDouble
#field nu , CDouble
#field p , CDouble
#field shrinking , CInt
#field probability , CInt
#stoptype


-- svm_model
#starttype struct svm_model
#field param , <svm_parameter>
#field nr_class , CInt
#field l , CInt
#field SV , Ptr (Ptr <svm_node>)
#field sv_coef , Ptr (Ptr CDouble)
#field rho , Ptr CDouble 	
#field probA , Ptr CDouble 	
#field probB , Ptr CDouble 	
#field sv_indices , Ptr CInt
#field label , Ptr CInt
#field nSV , Ptr CInt
#field free_sv , CInt
#stoptype

#ccall svm_train, Ptr <svm_problem> -> Ptr <svm_parameter> -> IO (Ptr <svm_model>)
#ccall svm_cross_validation, Ptr <svm_problem> -> Ptr <svm_parameter> -> CInt -> Ptr CDouble -> IO ()
#ccall svm_save_model , CString -> Ptr <svm_model> -> IO CInt
#ccall svm_load_model , CString -> IO (Ptr <svm_model>)
#ccall svm_predict , Ptr <svm_model> -> Ptr <svm_node> -> IO CDouble
#ccall svm_predict_probability , Ptr <svm_model> -> Ptr <svm_node> -> Ptr CDouble  -> IO CDouble
