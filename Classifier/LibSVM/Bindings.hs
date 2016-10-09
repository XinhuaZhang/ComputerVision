{-# LINE 1 "Bindings.hsc" #-}

{-# LINE 2 "Bindings.hsc" #-}

{-# LINE 3 "Bindings.hsc" #-}
         
module Classifier.LibSVM.Bindings where         
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

-- svm_node

{-# LINE 9 "Bindings.hsc" #-}

{-# LINE 10 "Bindings.hsc" #-}

{-# LINE 11 "Bindings.hsc" #-}
data C'svm_node = C'svm_node{
  c'svm_node'index :: CInt,
  c'svm_node'value :: CDouble
} deriving (Eq,Show)
p'svm_node'index p = plusPtr p 0
p'svm_node'index :: Ptr (C'svm_node) -> Ptr (CInt)
p'svm_node'value p = plusPtr p 8
p'svm_node'value :: Ptr (C'svm_node) -> Ptr (CDouble)
instance Storable C'svm_node where
  sizeOf _ = 16
  alignment _ = 8
  peek p = do
    v0 <- peekByteOff p 0
    v1 <- peekByteOff p 8
    return $ C'svm_node v0 v1
  poke p (C'svm_node v0 v1) = do
    pokeByteOff p 0 v0
    pokeByteOff p 8 v1
    return ()

{-# LINE 12 "Bindings.hsc" #-}

-- svm_problme

{-# LINE 15 "Bindings.hsc" #-}

{-# LINE 16 "Bindings.hsc" #-}

{-# LINE 17 "Bindings.hsc" #-}

{-# LINE 18 "Bindings.hsc" #-}
data C'svm_problem = C'svm_problem{
  c'svm_problem'l :: CInt,
  c'svm_problem'y :: Ptr CDouble,
  c'svm_problem'x :: Ptr (Ptr C'svm_node)
} deriving (Eq,Show)
p'svm_problem'l p = plusPtr p 0
p'svm_problem'l :: Ptr (C'svm_problem) -> Ptr (CInt)
p'svm_problem'y p = plusPtr p 8
p'svm_problem'y :: Ptr (C'svm_problem) -> Ptr (Ptr CDouble)
p'svm_problem'x p = plusPtr p 16
p'svm_problem'x :: Ptr (C'svm_problem) -> Ptr (Ptr (Ptr C'svm_node))
instance Storable C'svm_problem where
  sizeOf _ = 24
  alignment _ = 8
  peek p = do
    v0 <- peekByteOff p 0
    v1 <- peekByteOff p 8
    v2 <- peekByteOff p 16
    return $ C'svm_problem v0 v1 v2
  poke p (C'svm_problem v0 v1 v2) = do
    pokeByteOff p 0 v0
    pokeByteOff p 8 v1
    pokeByteOff p 16 v2
    return ()

{-# LINE 19 "Bindings.hsc" #-}

-- svm_type
c'C_SVC = 0
c'C_SVC :: (Num a) => a

{-# LINE 22 "Bindings.hsc" #-}
c'NU_SVC = 1
c'NU_SVC :: (Num a) => a

{-# LINE 23 "Bindings.hsc" #-}
c'ONE_CLASS = 2
c'ONE_CLASS :: (Num a) => a

{-# LINE 24 "Bindings.hsc" #-}
c'EPSILON_SVR = 3
c'EPSILON_SVR :: (Num a) => a

{-# LINE 25 "Bindings.hsc" #-}
c'NU_SVR = 4
c'NU_SVR :: (Num a) => a

{-# LINE 26 "Bindings.hsc" #-}

--kernel_type 
c'LINEAR = 0
c'LINEAR :: (Num a) => a

{-# LINE 29 "Bindings.hsc" #-}
c'POLY = 1
c'POLY :: (Num a) => a

{-# LINE 30 "Bindings.hsc" #-}
c'RBF = 2
c'RBF :: (Num a) => a

{-# LINE 31 "Bindings.hsc" #-}
c'SIGMOID = 3
c'SIGMOID :: (Num a) => a

{-# LINE 32 "Bindings.hsc" #-}
c'PRECOMPUTED = 4
c'PRECOMPUTED :: (Num a) => a

{-# LINE 33 "Bindings.hsc" #-}

-- svm_parameter

{-# LINE 36 "Bindings.hsc" #-}

{-# LINE 37 "Bindings.hsc" #-}

{-# LINE 38 "Bindings.hsc" #-}

{-# LINE 39 "Bindings.hsc" #-}

{-# LINE 40 "Bindings.hsc" #-}

{-# LINE 41 "Bindings.hsc" #-}

{-# LINE 42 "Bindings.hsc" #-}

{-# LINE 43 "Bindings.hsc" #-}

{-# LINE 44 "Bindings.hsc" #-}

{-# LINE 45 "Bindings.hsc" #-}

{-# LINE 46 "Bindings.hsc" #-}

{-# LINE 47 "Bindings.hsc" #-}

{-# LINE 48 "Bindings.hsc" #-}

{-# LINE 49 "Bindings.hsc" #-}

{-# LINE 50 "Bindings.hsc" #-}

{-# LINE 51 "Bindings.hsc" #-}
data C'svm_parameter = C'svm_parameter{
  c'svm_parameter'svm_type :: CInt,
  c'svm_parameter'kernel_type :: CInt,
  c'svm_parameter'degree :: CInt,
  c'svm_parameter'gamma :: CDouble,
  c'svm_parameter'coef0 :: CDouble,
  c'svm_parameter'cache_size :: CDouble,
  c'svm_parameter'eps :: CDouble,
  c'svm_parameter'C :: CDouble,
  c'svm_parameter'nr_weight :: CInt,
  c'svm_parameter'weight_label :: Ptr CInt,
  c'svm_parameter'weight :: Ptr CDouble,
  c'svm_parameter'nu :: CDouble,
  c'svm_parameter'p :: CDouble,
  c'svm_parameter'shrinking :: CInt,
  c'svm_parameter'probability :: CInt
} deriving (Eq,Show)
p'svm_parameter'svm_type p = plusPtr p 0
p'svm_parameter'svm_type :: Ptr (C'svm_parameter) -> Ptr (CInt)
p'svm_parameter'kernel_type p = plusPtr p 4
p'svm_parameter'kernel_type :: Ptr (C'svm_parameter) -> Ptr (CInt)
p'svm_parameter'degree p = plusPtr p 8
p'svm_parameter'degree :: Ptr (C'svm_parameter) -> Ptr (CInt)
p'svm_parameter'gamma p = plusPtr p 16
p'svm_parameter'gamma :: Ptr (C'svm_parameter) -> Ptr (CDouble)
p'svm_parameter'coef0 p = plusPtr p 24
p'svm_parameter'coef0 :: Ptr (C'svm_parameter) -> Ptr (CDouble)
p'svm_parameter'cache_size p = plusPtr p 32
p'svm_parameter'cache_size :: Ptr (C'svm_parameter) -> Ptr (CDouble)
p'svm_parameter'eps p = plusPtr p 40
p'svm_parameter'eps :: Ptr (C'svm_parameter) -> Ptr (CDouble)
p'svm_parameter'C p = plusPtr p 48
p'svm_parameter'C :: Ptr (C'svm_parameter) -> Ptr (CDouble)
p'svm_parameter'nr_weight p = plusPtr p 56
p'svm_parameter'nr_weight :: Ptr (C'svm_parameter) -> Ptr (CInt)
p'svm_parameter'weight_label p = plusPtr p 64
p'svm_parameter'weight_label :: Ptr (C'svm_parameter) -> Ptr (Ptr CInt)
p'svm_parameter'weight p = plusPtr p 72
p'svm_parameter'weight :: Ptr (C'svm_parameter) -> Ptr (Ptr CDouble)
p'svm_parameter'nu p = plusPtr p 80
p'svm_parameter'nu :: Ptr (C'svm_parameter) -> Ptr (CDouble)
p'svm_parameter'p p = plusPtr p 88
p'svm_parameter'p :: Ptr (C'svm_parameter) -> Ptr (CDouble)
p'svm_parameter'shrinking p = plusPtr p 96
p'svm_parameter'shrinking :: Ptr (C'svm_parameter) -> Ptr (CInt)
p'svm_parameter'probability p = plusPtr p 100
p'svm_parameter'probability :: Ptr (C'svm_parameter) -> Ptr (CInt)
instance Storable C'svm_parameter where
  sizeOf _ = 104
  alignment _ = 8
  peek p = do
    v0 <- peekByteOff p 0
    v1 <- peekByteOff p 4
    v2 <- peekByteOff p 8
    v3 <- peekByteOff p 16
    v4 <- peekByteOff p 24
    v5 <- peekByteOff p 32
    v6 <- peekByteOff p 40
    v7 <- peekByteOff p 48
    v8 <- peekByteOff p 56
    v9 <- peekByteOff p 64
    v10 <- peekByteOff p 72
    v11 <- peekByteOff p 80
    v12 <- peekByteOff p 88
    v13 <- peekByteOff p 96
    v14 <- peekByteOff p 100
    return $ C'svm_parameter v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14
  poke p (C'svm_parameter v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14) = do
    pokeByteOff p 0 v0
    pokeByteOff p 4 v1
    pokeByteOff p 8 v2
    pokeByteOff p 16 v3
    pokeByteOff p 24 v4
    pokeByteOff p 32 v5
    pokeByteOff p 40 v6
    pokeByteOff p 48 v7
    pokeByteOff p 56 v8
    pokeByteOff p 64 v9
    pokeByteOff p 72 v10
    pokeByteOff p 80 v11
    pokeByteOff p 88 v12
    pokeByteOff p 96 v13
    pokeByteOff p 100 v14
    return ()

{-# LINE 52 "Bindings.hsc" #-}


-- svm_model

{-# LINE 56 "Bindings.hsc" #-}

{-# LINE 57 "Bindings.hsc" #-}

{-# LINE 58 "Bindings.hsc" #-}

{-# LINE 59 "Bindings.hsc" #-}

{-# LINE 60 "Bindings.hsc" #-}

{-# LINE 61 "Bindings.hsc" #-}

{-# LINE 62 "Bindings.hsc" #-}

{-# LINE 63 "Bindings.hsc" #-}

{-# LINE 64 "Bindings.hsc" #-}

{-# LINE 65 "Bindings.hsc" #-}

{-# LINE 66 "Bindings.hsc" #-}

{-# LINE 67 "Bindings.hsc" #-}

{-# LINE 68 "Bindings.hsc" #-}
data C'svm_model = C'svm_model{
  c'svm_model'param :: C'svm_parameter,
  c'svm_model'nr_class :: CInt,
  c'svm_model'l :: CInt,
  c'svm_model'SV :: Ptr (Ptr C'svm_node),
  c'svm_model'sv_coef :: Ptr (Ptr CDouble),
  c'svm_model'rho :: Ptr CDouble,
  c'svm_model'probA :: Ptr CDouble,
  c'svm_model'probB :: Ptr CDouble,
  c'svm_model'sv_indices :: Ptr CInt,
  c'svm_model'label :: Ptr CInt,
  c'svm_model'nSV :: Ptr CInt,
  c'svm_model'free_sv :: CInt
} deriving (Eq,Show)
p'svm_model'param p = plusPtr p 0
p'svm_model'param :: Ptr (C'svm_model) -> Ptr (C'svm_parameter)
p'svm_model'nr_class p = plusPtr p 104
p'svm_model'nr_class :: Ptr (C'svm_model) -> Ptr (CInt)
p'svm_model'l p = plusPtr p 108
p'svm_model'l :: Ptr (C'svm_model) -> Ptr (CInt)
p'svm_model'SV p = plusPtr p 112
p'svm_model'SV :: Ptr (C'svm_model) -> Ptr (Ptr (Ptr C'svm_node))
p'svm_model'sv_coef p = plusPtr p 120
p'svm_model'sv_coef :: Ptr (C'svm_model) -> Ptr (Ptr (Ptr CDouble))
p'svm_model'rho p = plusPtr p 128
p'svm_model'rho :: Ptr (C'svm_model) -> Ptr (Ptr CDouble)
p'svm_model'probA p = plusPtr p 136
p'svm_model'probA :: Ptr (C'svm_model) -> Ptr (Ptr CDouble)
p'svm_model'probB p = plusPtr p 144
p'svm_model'probB :: Ptr (C'svm_model) -> Ptr (Ptr CDouble)
p'svm_model'sv_indices p = plusPtr p 152
p'svm_model'sv_indices :: Ptr (C'svm_model) -> Ptr (Ptr CInt)
p'svm_model'label p = plusPtr p 160
p'svm_model'label :: Ptr (C'svm_model) -> Ptr (Ptr CInt)
p'svm_model'nSV p = plusPtr p 168
p'svm_model'nSV :: Ptr (C'svm_model) -> Ptr (Ptr CInt)
p'svm_model'free_sv p = plusPtr p 176
p'svm_model'free_sv :: Ptr (C'svm_model) -> Ptr (CInt)
instance Storable C'svm_model where
  sizeOf _ = 184
  alignment _ = 8
  peek p = do
    v0 <- peekByteOff p 0
    v1 <- peekByteOff p 104
    v2 <- peekByteOff p 108
    v3 <- peekByteOff p 112
    v4 <- peekByteOff p 120
    v5 <- peekByteOff p 128
    v6 <- peekByteOff p 136
    v7 <- peekByteOff p 144
    v8 <- peekByteOff p 152
    v9 <- peekByteOff p 160
    v10 <- peekByteOff p 168
    v11 <- peekByteOff p 176
    return $ C'svm_model v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11
  poke p (C'svm_model v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11) = do
    pokeByteOff p 0 v0
    pokeByteOff p 104 v1
    pokeByteOff p 108 v2
    pokeByteOff p 112 v3
    pokeByteOff p 120 v4
    pokeByteOff p 128 v5
    pokeByteOff p 136 v6
    pokeByteOff p 144 v7
    pokeByteOff p 152 v8
    pokeByteOff p 160 v9
    pokeByteOff p 168 v10
    pokeByteOff p 176 v11
    return ()

{-# LINE 69 "Bindings.hsc" #-}

foreign import ccall "svm_train" c'svm_train
  :: Ptr C'svm_problem -> Ptr C'svm_parameter -> IO (Ptr C'svm_model)
foreign import ccall "&svm_train" p'svm_train
  :: FunPtr (Ptr C'svm_problem -> Ptr C'svm_parameter -> IO (Ptr C'svm_model))

{-# LINE 71 "Bindings.hsc" #-}
foreign import ccall "svm_cross_validation" c'svm_cross_validation
  :: Ptr C'svm_problem -> Ptr C'svm_parameter -> CInt -> Ptr CDouble -> IO ()
foreign import ccall "&svm_cross_validation" p'svm_cross_validation
  :: FunPtr (Ptr C'svm_problem -> Ptr C'svm_parameter -> CInt -> Ptr CDouble -> IO ())

{-# LINE 72 "Bindings.hsc" #-}
foreign import ccall "svm_save_model" c'svm_save_model
  :: CString -> Ptr C'svm_model -> IO CInt
foreign import ccall "&svm_save_model" p'svm_save_model
  :: FunPtr (CString -> Ptr C'svm_model -> IO CInt)

{-# LINE 73 "Bindings.hsc" #-}
foreign import ccall "svm_load_model" c'svm_load_model
  :: CString -> IO (Ptr C'svm_model)
foreign import ccall "&svm_load_model" p'svm_load_model
  :: FunPtr (CString -> IO (Ptr C'svm_model))

{-# LINE 74 "Bindings.hsc" #-}
foreign import ccall "svm_predict" c'svm_predict
  :: Ptr C'svm_model -> Ptr C'svm_node -> IO CDouble
foreign import ccall "&svm_predict" p'svm_predict
  :: FunPtr (Ptr C'svm_model -> Ptr C'svm_node -> IO CDouble)

{-# LINE 75 "Bindings.hsc" #-}
foreign import ccall "svm_predict_probability" c'svm_predict_probability
  :: Ptr C'svm_model -> Ptr C'svm_node -> Ptr CDouble -> IO CDouble
foreign import ccall "&svm_predict_probability" p'svm_predict_probability
  :: FunPtr (Ptr C'svm_model -> Ptr C'svm_node -> Ptr CDouble -> IO CDouble)

{-# LINE 76 "Bindings.hsc" #-}
