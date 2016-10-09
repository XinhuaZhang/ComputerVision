{-# LINE 1 "Bindings.hsc" #-}
{-|
{-# LINE 2 "Bindings.hsc" #-}
For a high-level description of the C API, refer to the README file
included in the liblinear archive, available for download at
<http://www.csie.ntu.edu.tw/~cjlin/liblinear/>.
-}


{-# LINE 8 "Bindings.hsc" #-}

{-# LINE 9 "Bindings.hsc" #-}

module Classifier.LibLinear.Bindings where
import Foreign.Ptr (Ptr,FunPtr,plusPtr)
import Foreign.Ptr (wordPtrToPtr,castPtrToFunPtr)
import Foreign.Storable
import Foreign.C.Types
import Foreign.C.String (CString,CStringLen,CWString,CWStringLen)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (peekArray,pokeArray)
import Data.Int
import Data.Word

{-# LINE 12 "Bindings.hsc" #-}

-- feature_node

{-# LINE 15 "Bindings.hsc" #-}

{-# LINE 16 "Bindings.hsc" #-}

{-# LINE 17 "Bindings.hsc" #-}
data C'feature_node = C'feature_node{
  c'feature_node'index :: CInt,
  c'feature_node'value :: CDouble
} deriving (Eq,Show)
p'feature_node'index p = plusPtr p 0
p'feature_node'index :: Ptr (C'feature_node) -> Ptr (CInt)
p'feature_node'value p = plusPtr p 8
p'feature_node'value :: Ptr (C'feature_node) -> Ptr (CDouble)
instance Storable C'feature_node where
  sizeOf _ = 16
  alignment _ = 8
  peek p = do
    v0 <- peekByteOff p 0
    v1 <- peekByteOff p 8
    return $ C'feature_node v0 v1
  poke p (C'feature_node v0 v1) = do
    pokeByteOff p 0 v0
    pokeByteOff p 8 v1
    return ()

{-# LINE 18 "Bindings.hsc" #-}

-- problem

{-# LINE 21 "Bindings.hsc" #-}

{-# LINE 22 "Bindings.hsc" #-}

{-# LINE 23 "Bindings.hsc" #-}

{-# LINE 24 "Bindings.hsc" #-}

{-# LINE 25 "Bindings.hsc" #-}

{-# LINE 26 "Bindings.hsc" #-}
data C'problem = C'problem{
  c'problem'l :: CInt,
  c'problem'n :: CInt,
  c'problem'y :: Ptr CDouble,
  c'problem'x :: Ptr (Ptr C'feature_node),
  c'problem'bias :: CDouble
} deriving (Eq,Show)
p'problem'l p = plusPtr p 0
p'problem'l :: Ptr (C'problem) -> Ptr (CInt)
p'problem'n p = plusPtr p 4
p'problem'n :: Ptr (C'problem) -> Ptr (CInt)
p'problem'y p = plusPtr p 8
p'problem'y :: Ptr (C'problem) -> Ptr (Ptr CDouble)
p'problem'x p = plusPtr p 16
p'problem'x :: Ptr (C'problem) -> Ptr (Ptr (Ptr C'feature_node))
p'problem'bias p = plusPtr p 24
p'problem'bias :: Ptr (C'problem) -> Ptr (CDouble)
instance Storable C'problem where
  sizeOf _ = 32
  alignment _ = 8
  peek p = do
    v0 <- peekByteOff p 0
    v1 <- peekByteOff p 4
    v2 <- peekByteOff p 8
    v3 <- peekByteOff p 16
    v4 <- peekByteOff p 24
    return $ C'problem v0 v1 v2 v3 v4
  poke p (C'problem v0 v1 v2 v3 v4) = do
    pokeByteOff p 0 v0
    pokeByteOff p 4 v1
    pokeByteOff p 8 v2
    pokeByteOff p 16 v3
    pokeByteOff p 24 v4
    return ()

{-# LINE 27 "Bindings.hsc" #-}

-- solver_type
c'L2R_LR = 0
c'L2R_LR :: (Num a) => a

{-# LINE 30 "Bindings.hsc" #-}
c'L2R_L2LOSS_SVC_DUAL = 1
c'L2R_L2LOSS_SVC_DUAL :: (Num a) => a

{-# LINE 31 "Bindings.hsc" #-}
c'L2R_L2LOSS_SVC = 2
c'L2R_L2LOSS_SVC :: (Num a) => a

{-# LINE 32 "Bindings.hsc" #-}
c'L2R_L1LOSS_SVC_DUAL = 3
c'L2R_L1LOSS_SVC_DUAL :: (Num a) => a

{-# LINE 33 "Bindings.hsc" #-}
c'MCSVM_CS = 4
c'MCSVM_CS :: (Num a) => a

{-# LINE 34 "Bindings.hsc" #-}
c'L1R_L2LOSS_SVC = 5
c'L1R_L2LOSS_SVC :: (Num a) => a

{-# LINE 35 "Bindings.hsc" #-}
c'L1R_LR = 6
c'L1R_LR :: (Num a) => a

{-# LINE 36 "Bindings.hsc" #-}
c'L2R_LR_DUAL = 7
c'L2R_LR_DUAL :: (Num a) => a

{-# LINE 37 "Bindings.hsc" #-}
c'L2R_L2LOSS_SVR = 11
c'L2R_L2LOSS_SVR :: (Num a) => a

{-# LINE 38 "Bindings.hsc" #-}
c'L2R_L2LOSS_SVR_DUAL = 12
c'L2R_L2LOSS_SVR_DUAL :: (Num a) => a

{-# LINE 39 "Bindings.hsc" #-}
c'L2R_L1LOSS_SVR_DUAL = 13
c'L2R_L1LOSS_SVR_DUAL :: (Num a) => a

{-# LINE 40 "Bindings.hsc" #-}
-- parameter

{-# LINE 42 "Bindings.hsc" #-}

{-# LINE 43 "Bindings.hsc" #-}

{-# LINE 44 "Bindings.hsc" #-}

{-# LINE 45 "Bindings.hsc" #-}

{-# LINE 46 "Bindings.hsc" #-}

{-# LINE 47 "Bindings.hsc" #-}

{-# LINE 48 "Bindings.hsc" #-}

{-# LINE 49 "Bindings.hsc" #-}

{-# LINE 50 "Bindings.hsc" #-}
data C'parameter = C'parameter{
  c'parameter'solver_type :: CInt,
  c'parameter'eps :: CDouble,
  c'parameter'C :: CDouble,
  c'parameter'nr_weight :: CInt,
  c'parameter'weight_label :: Ptr CInt,
  c'parameter'weight :: Ptr CDouble,
  c'parameter'p :: CDouble,
  c'parameter'init_sol :: Ptr CDouble
} deriving (Eq,Show)
p'parameter'solver_type p = plusPtr p 0
p'parameter'solver_type :: Ptr (C'parameter) -> Ptr (CInt)
p'parameter'eps p = plusPtr p 8
p'parameter'eps :: Ptr (C'parameter) -> Ptr (CDouble)
p'parameter'C p = plusPtr p 16
p'parameter'C :: Ptr (C'parameter) -> Ptr (CDouble)
p'parameter'nr_weight p = plusPtr p 24
p'parameter'nr_weight :: Ptr (C'parameter) -> Ptr (CInt)
p'parameter'weight_label p = plusPtr p 32
p'parameter'weight_label :: Ptr (C'parameter) -> Ptr (Ptr CInt)
p'parameter'weight p = plusPtr p 40
p'parameter'weight :: Ptr (C'parameter) -> Ptr (Ptr CDouble)
p'parameter'p p = plusPtr p 48
p'parameter'p :: Ptr (C'parameter) -> Ptr (CDouble)
p'parameter'init_sol p = plusPtr p 56
p'parameter'init_sol :: Ptr (C'parameter) -> Ptr (Ptr CDouble)
instance Storable C'parameter where
  sizeOf _ = 64
  alignment _ = 8
  peek p = do
    v0 <- peekByteOff p 0
    v1 <- peekByteOff p 8
    v2 <- peekByteOff p 16
    v3 <- peekByteOff p 24
    v4 <- peekByteOff p 32
    v5 <- peekByteOff p 40
    v6 <- peekByteOff p 48
    v7 <- peekByteOff p 56
    return $ C'parameter v0 v1 v2 v3 v4 v5 v6 v7
  poke p (C'parameter v0 v1 v2 v3 v4 v5 v6 v7) = do
    pokeByteOff p 0 v0
    pokeByteOff p 8 v1
    pokeByteOff p 16 v2
    pokeByteOff p 24 v3
    pokeByteOff p 32 v4
    pokeByteOff p 40 v5
    pokeByteOff p 48 v6
    pokeByteOff p 56 v7
    return ()

{-# LINE 51 "Bindings.hsc" #-}

-- model

{-# LINE 54 "Bindings.hsc" #-}

{-# LINE 55 "Bindings.hsc" #-}

{-# LINE 56 "Bindings.hsc" #-}

{-# LINE 57 "Bindings.hsc" #-}

{-# LINE 58 "Bindings.hsc" #-}

{-# LINE 59 "Bindings.hsc" #-}

{-# LINE 60 "Bindings.hsc" #-}
data C'model = C'model{
  c'model'param :: C'parameter,
  c'model'nr_class :: CInt,
  c'model'nr_feature :: CInt,
  c'model'w :: Ptr CDouble,
  c'model'label :: Ptr CInt,
  c'model'bias :: CDouble
} deriving (Eq,Show)
p'model'param p = plusPtr p 0
p'model'param :: Ptr (C'model) -> Ptr (C'parameter)
p'model'nr_class p = plusPtr p 64
p'model'nr_class :: Ptr (C'model) -> Ptr (CInt)
p'model'nr_feature p = plusPtr p 68
p'model'nr_feature :: Ptr (C'model) -> Ptr (CInt)
p'model'w p = plusPtr p 72
p'model'w :: Ptr (C'model) -> Ptr (Ptr CDouble)
p'model'label p = plusPtr p 80
p'model'label :: Ptr (C'model) -> Ptr (Ptr CInt)
p'model'bias p = plusPtr p 88
p'model'bias :: Ptr (C'model) -> Ptr (CDouble)
instance Storable C'model where
  sizeOf _ = 96
  alignment _ = 8
  peek p = do
    v0 <- peekByteOff p 0
    v1 <- peekByteOff p 64
    v2 <- peekByteOff p 68
    v3 <- peekByteOff p 72
    v4 <- peekByteOff p 80
    v5 <- peekByteOff p 88
    return $ C'model v0 v1 v2 v3 v4 v5
  poke p (C'model v0 v1 v2 v3 v4 v5) = do
    pokeByteOff p 0 v0
    pokeByteOff p 64 v1
    pokeByteOff p 68 v2
    pokeByteOff p 72 v3
    pokeByteOff p 80 v4
    pokeByteOff p 88 v5
    return ()

{-# LINE 61 "Bindings.hsc" #-}

-- training
foreign import ccall "train" c'train
  :: Ptr C'problem -> Ptr C'parameter -> IO (Ptr C'model)
foreign import ccall "&train" p'train
  :: FunPtr (Ptr C'problem -> Ptr C'parameter -> IO (Ptr C'model))

{-# LINE 64 "Bindings.hsc" #-}

-- cross validation
foreign import ccall "cross_validation" c'cross_validation
  :: Ptr C'problem -> Ptr C'parameter -> CInt -> Ptr CDouble -> IO ()
foreign import ccall "&cross_validation" p'cross_validation
  :: FunPtr (Ptr C'problem -> Ptr C'parameter -> CInt -> Ptr CDouble -> IO ())

{-# LINE 67 "Bindings.hsc" #-}

-- find parameter C
foreign import ccall "find_parameter_C" c'find_parameter_C
  :: Ptr C'problem -> Ptr C'parameter -> CInt -> CDouble -> CDouble -> Ptr CDouble -> Ptr CDouble -> IO ()
foreign import ccall "&find_parameter_C" p'find_parameter_C
  :: FunPtr (Ptr C'problem -> Ptr C'parameter -> CInt -> CDouble -> CDouble -> Ptr CDouble -> Ptr CDouble -> IO ())

{-# LINE 70 "Bindings.hsc" #-}

-- saving models
foreign import ccall "save_model" c'save_model
  :: CString -> Ptr C'model -> IO ()
foreign import ccall "&save_model" p'save_model
  :: FunPtr (CString -> Ptr C'model -> IO ())

{-# LINE 73 "Bindings.hsc" #-}

-- loading models
foreign import ccall "load_model" c'load_model
  :: CString -> IO (Ptr C'model)
foreign import ccall "&load_model" p'load_model
  :: FunPtr (CString -> IO (Ptr C'model))

{-# LINE 76 "Bindings.hsc" #-}

-- getting properties
foreign import ccall "get_nr_feature" c'get_nr_feature
  :: Ptr C'model -> IO CInt
foreign import ccall "&get_nr_feature" p'get_nr_feature
  :: FunPtr (Ptr C'model -> IO CInt)

{-# LINE 79 "Bindings.hsc" #-}
foreign import ccall "get_nr_class" c'get_nr_class
  :: Ptr C'model -> IO CInt
foreign import ccall "&get_nr_class" p'get_nr_class
  :: FunPtr (Ptr C'model -> IO CInt)

{-# LINE 80 "Bindings.hsc" #-}
foreign import ccall "get_labels" c'get_labels
  :: Ptr C'model -> Ptr CInt -> IO ()
foreign import ccall "&get_labels" p'get_labels
  :: FunPtr (Ptr C'model -> Ptr CInt -> IO ())

{-# LINE 81 "Bindings.hsc" #-}

-- predictions
foreign import ccall "predict_values" c'predict_values
  :: Ptr C'model -> Ptr C'feature_node -> Ptr CDouble -> IO CDouble
foreign import ccall "&predict_values" p'predict_values
  :: FunPtr (Ptr C'model -> Ptr C'feature_node -> Ptr CDouble -> IO CDouble)

{-# LINE 84 "Bindings.hsc" #-}
foreign import ccall "predict" c'predict
  :: Ptr C'model -> Ptr C'feature_node -> IO CDouble
foreign import ccall "&predict" p'predict
  :: FunPtr (Ptr C'model -> Ptr C'feature_node -> IO CDouble)

{-# LINE 85 "Bindings.hsc" #-}
foreign import ccall "predict_probability" c'predict_probability
  :: Ptr C'model -> Ptr C'feature_node -> Ptr CDouble -> IO CDouble
foreign import ccall "&predict_probability" p'predict_probability
  :: FunPtr (Ptr C'model -> Ptr C'feature_node -> Ptr CDouble -> IO CDouble)

{-# LINE 86 "Bindings.hsc" #-}

-- destroying
foreign import ccall "free_model_content" c'free_model_content
  :: Ptr C'model -> IO ()
foreign import ccall "&free_model_content" p'free_model_content
  :: FunPtr (Ptr C'model -> IO ())

{-# LINE 89 "Bindings.hsc" #-}
foreign import ccall "free_and_destroy_model" c'free_and_destroy_model
  :: Ptr (Ptr C'model) -> IO ()
foreign import ccall "&free_and_destroy_model" p'free_and_destroy_model
  :: FunPtr (Ptr (Ptr C'model) -> IO ())

{-# LINE 90 "Bindings.hsc" #-}
foreign import ccall "destroy_param" c'destroy_param
  :: Ptr C'parameter -> IO ()
foreign import ccall "&destroy_param" p'destroy_param
  :: FunPtr (Ptr C'parameter -> IO ())

{-# LINE 91 "Bindings.hsc" #-}

-- checking
foreign import ccall "check_parameter" c'check_parameter
  :: Ptr C'problem -> Ptr C'parameter -> IO CString
foreign import ccall "&check_parameter" p'check_parameter
  :: FunPtr (Ptr C'problem -> Ptr C'parameter -> IO CString)

{-# LINE 94 "Bindings.hsc" #-}
foreign import ccall "check_probability_model" c'check_probability_model
  :: Ptr C'model -> IO CInt
foreign import ccall "&check_probability_model" p'check_probability_model
  :: FunPtr (Ptr C'model -> IO CInt)

{-# LINE 95 "Bindings.hsc" #-}
foreign import ccall "check_regression_model" c'check_regression_model
  :: Ptr C'model -> IO CInt
foreign import ccall "&check_regression_model" p'check_regression_model
  :: FunPtr (Ptr C'model -> IO CInt)

{-# LINE 96 "Bindings.hsc" #-}
-- #ccall set_print_string_function , FunPtr (CString -> IO ())

-- printing
-- #ccall svm_print_string , FunPtr (CString -> IO ())
