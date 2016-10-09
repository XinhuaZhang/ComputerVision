{-|
For a high-level description of the C API, refer to the README file
included in the liblinear archive, available for download at
<http://www.csie.ntu.edu.tw/~cjlin/liblinear/>.
-}

#include "bindings.dsl.h"
#include "linear.h"

module Classifier.LibLinear.Bindings where
#strict_import

-- feature_node
#starttype struct feature_node
#field index , CInt
#field value , CDouble
#stoptype

-- problem
#starttype struct problem
#field l , CInt
#field n , CInt
#field y , Ptr CDouble
#field x , Ptr (Ptr <feature_node>)
#field bias , CDouble
#stoptype

-- solver_type
#num L2R_LR
#num L2R_L2LOSS_SVC_DUAL
#num L2R_L2LOSS_SVC
#num L2R_L1LOSS_SVC_DUAL
#num MCSVM_CS
#num L1R_L2LOSS_SVC
#num L1R_LR
#num L2R_LR_DUAL
#num L2R_L2LOSS_SVR  
#num L2R_L2LOSS_SVR_DUAL
#num L2R_L1LOSS_SVR_DUAL
-- parameter
#starttype struct parameter
#field solver_type , CInt
#field eps , CDouble
#field C , CDouble
#field nr_weight , CInt
#field weight_label , Ptr CInt
#field weight , Ptr CDouble
#field p , CDouble
#field init_sol , Ptr CDouble
#stoptype

-- model
#starttype struct model
#field param , <parameter>
#field nr_class , CInt
#field nr_feature , CInt
#field w , Ptr CDouble
#field label , Ptr CInt
#field bias , CDouble
#stoptype

-- training
#ccall train , Ptr <problem> -> Ptr <parameter> -> IO (Ptr <model>)

-- cross validation
#ccall cross_validation , Ptr <problem> -> Ptr <parameter> -> CInt -> Ptr CDouble -> IO ()

-- find parameter C
#ccall find_parameter_C , Ptr <problem> -> Ptr <parameter> -> CInt -> CDouble -> CDouble -> Ptr CDouble -> Ptr CDouble -> IO ()

-- saving models
#ccall save_model , CString -> Ptr <model> -> IO ()

-- loading models
#ccall load_model , CString -> IO (Ptr <model>)

-- getting properties
#ccall get_nr_feature , Ptr <model> -> IO CInt
#ccall get_nr_class , Ptr <model> -> IO CInt
#ccall get_labels , Ptr <model> -> Ptr CInt -> IO ()

-- predictions
#ccall predict_values , Ptr <model> -> Ptr <feature_node> -> Ptr CDouble -> IO CDouble
#ccall predict , Ptr <model> -> Ptr <feature_node> -> IO CDouble
#ccall predict_probability , Ptr <model> -> Ptr <feature_node> -> Ptr CDouble -> IO CDouble

-- destroying
#ccall free_model_content , Ptr <model> -> IO ()
#ccall free_and_destroy_model , Ptr (Ptr <model>) -> IO ()
#ccall destroy_param , Ptr <parameter> -> IO ()

-- checking
#ccall check_parameter , Ptr <problem> -> Ptr <parameter> -> IO CString
#ccall check_probability_model , Ptr <model> -> IO CInt
#ccall check_regression_model , Ptr <model> -> IO CInt
-- #ccall set_print_string_function , FunPtr (CString -> IO ())

-- printing
-- #ccall svm_print_string , FunPtr (CString -> IO ())
