{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE ForeignFunctionInterface   #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE PatternGuards              #-}
module Classifier.LibSVM.Interface where

import           Classifier.LibSVM.Bindings
import           Control.Monad              (liftM)
import           Control.Monad.IO.Class     (liftIO)
import           Data.Conduit
import           Data.Conduit.List          as CL
import           Data.Data
import           Data.Image
import qualified Data.List                  as L
import           Data.Ord
import qualified Data.Vector                as V
import qualified Data.Vector.Unboxed        as VU
import           Foreign                    as F
import           Foreign.C
import           Foreign.Marshal.Array
import           Foreign.Ptr
import           Prelude                    as P
import           System.IO
import           Control.Monad.Trans.Resource

data Kernel
  = LINEAR
  | POLY
  | RBF
  | SIGMOID
  | PRECOMPUTED
  deriving (Show,Eq,Data,Typeable)

instance Bounded Kernel where
  minBound = LINEAR
  maxBound = PRECOMPUTED

instance Enum Kernel where
  fromEnum LINEAR = c'LINEAR
  fromEnum POLY = c'POLY
  fromEnum RBF = c'RBF
  fromEnum SIGMOID = c'SIGMOID
  fromEnum PRECOMPUTED = c'PRECOMPUTED
  toEnum v
    | v <= c'LINEAR = LINEAR
    | v == c'POLY = POLY
    | v == c'RBF = RBF
    | v == c'SIGMOID = SIGMOID
    | v == c'PRECOMPUTED = PRECOMPUTED
    | otherwise = maxBound

data Type
  = C_SVC
  | NU_SVC
  | ONE_CLASS
  | EPSILON_SVR
  | NU_SVR
  deriving (Show,Eq,Data,Typeable)

instance Bounded Type where
  minBound = C_SVC
  maxBound = NU_SVR

instance Enum Type where
  fromEnum C_SVC = c'C_SVC
  fromEnum NU_SVC = c'NU_SVC
  fromEnum ONE_CLASS = c'ONE_CLASS
  fromEnum EPSILON_SVR = c'EPSILON_SVR
  fromEnum NU_SVR = c'NU_SVR
  toEnum v
    | v <= c'C_SVC = C_SVC
    | v == c'NU_SVC = NU_SVC
    | v == c'ONE_CLASS = ONE_CLASS
    | v == c'EPSILON_SVR = EPSILON_SVR
    | v == c'NU_SVR = NU_SVR
    | otherwise = maxBound

data TrainParams =
  TrainParams {svmType    :: Type
              ,kernelType :: Kernel
              ,modelName  :: String
              ,numFeature :: Int
              ,c          :: Double
              ,eps        :: Double
              ,nu         ::Double}
  deriving (Show)

train
  :: TrainParams -> [Double] -> [Ptr C'svm_node] -> IO CInt
train (TrainParams svmType kernelType modelName numFeature c eps nu) labels features =
  do labelPtr <- newArray . P.map realToFrac $ labels
     featurePtr <- newArray features
     let params =
           C'svm_parameter {c'svm_parameter'svm_type =
                              fromIntegral . fromEnum $ svmType
                           ,c'svm_parameter'kernel_type =
                              fromIntegral . fromEnum $ kernelType
                           ,c'svm_parameter'degree = 3
                           ,c'svm_parameter'gamma =
                              1 / (fromIntegral numFeature)
                           ,c'svm_parameter'coef0 = 0
                           ,c'svm_parameter'cache_size = 100
                           ,c'svm_parameter'eps = realToFrac eps
                           ,c'svm_parameter'C = realToFrac c
                           ,c'svm_parameter'nr_weight = 0
                           ,c'svm_parameter'weight_label = nullPtr
                           ,c'svm_parameter'weight = nullPtr
                           ,c'svm_parameter'nu = realToFrac nu
                           ,c'svm_parameter'p = 0.1
                           ,c'svm_parameter'shrinking = 1
                           ,c'svm_parameter'probability = 0}
         problem =
           C'svm_problem {c'svm_problem'l = fromIntegral . length $ features
                         ,c'svm_problem'y = labelPtr
                         ,c'svm_problem'x = featurePtr}
     model <-
       with problem
            (\problem' ->
               with params (\params' -> c'svm_train problem' params'))
     modelName' <- newCString modelName
     c'svm_save_model modelName' model

predict
  :: String -> FilePath -> Sink (Double,Ptr C'svm_node) IO ()
predict modelName output =
  do modelNameCS <- liftIO $ newCString modelName
     model <- liftIO $ c'svm_load_model modelNameCS
     (correct,total) <-
       CL.foldM (func model)
                (0,0)
     let percent = (fromIntegral correct) / (fromIntegral total) * 100
         str = show percent
     liftIO $ putStrLn str
     h <- liftIO $ openFile output WriteMode
     liftIO $ hPutStrLn h str
     liftIO $ hClose h
  where func :: Ptr C'svm_model
             -> (Int,Int)
             -> (Double,Ptr C'svm_node)
             -> IO (Int,Int)
        func model (correct,total) (!target,!featurePtr) =
          do prediction <- c'svm_predict model featurePtr
             if round target == round prediction
                then return (correct + 1,total + 1)
                else return (correct,total + 1)

crossValidation
  :: TrainParams -> [Double] -> [Ptr C'svm_node] -> Int -> IO ()
crossValidation (TrainParams svmType kernelType modelName numFeature c eps nu) labels features nrFold =
  do labelPtr <- newArray . P.map realToFrac $ labels
     featurePtr <- newArray features
     let params =
           C'svm_parameter {c'svm_parameter'svm_type =
                              fromIntegral . fromEnum $ svmType
                           ,c'svm_parameter'kernel_type =
                              fromIntegral . fromEnum $ kernelType
                           ,c'svm_parameter'degree = 3
                           ,c'svm_parameter'gamma =
                              1 / (fromIntegral numFeature)
                           ,c'svm_parameter'coef0 = 0
                           ,c'svm_parameter'cache_size = 100
                           ,c'svm_parameter'eps = realToFrac eps
                           ,c'svm_parameter'C = realToFrac c
                           ,c'svm_parameter'nr_weight = 0
                           ,c'svm_parameter'weight_label = nullPtr
                           ,c'svm_parameter'weight = nullPtr
                           ,c'svm_parameter'nu = realToFrac nu
                           ,c'svm_parameter'p = 0.1
                           ,c'svm_parameter'shrinking = 1
                           ,c'svm_parameter'probability = 0}
         problem =
           C'svm_problem {c'svm_problem'l = fromIntegral . length $ features
                         ,c'svm_problem'y = labelPtr
                         ,c'svm_problem'x = featurePtr}
     allocaArray
       (length features)
       (\target ->
          do with problem
                  (\problem' ->
                     with params
                          (\params' ->
                             c'svm_cross_validation problem'
                                                    params'
                                                    (fromIntegral nrFold)
                                                    target))
             prediction <- peekArray (length features) target
             let correct =
                   sum $
                   zipWith (\x y ->
                              if round x == round y
                                 then 1
                                 else 0)
                           prediction
                           labels
             putStrLn ("Cross Validation Accuracy = " P.++
                       (show $
                        (fromIntegral correct) /
                        (fromIntegral $ length features))))


oneVsRestTrain
  :: TrainParams -> [Double] -> [Ptr C'svm_node] -> IO ()
oneVsRestTrain (TrainParams svmType kernelType modelName numFeature c eps nu) labels features =
  do featurePtr <- newArray features
     let labelMax = P.maximum labels
         labelMin = P.minimum labels
         params =
           C'svm_parameter {c'svm_parameter'svm_type =
                              fromIntegral . fromEnum $ svmType
                           ,c'svm_parameter'kernel_type =
                              fromIntegral . fromEnum $ kernelType
                           ,c'svm_parameter'degree = 3
                           ,c'svm_parameter'gamma =
                              1 / (fromIntegral numFeature)
                           ,c'svm_parameter'coef0 = 0
                           ,c'svm_parameter'cache_size = 100
                           ,c'svm_parameter'eps = realToFrac eps
                           ,c'svm_parameter'C = realToFrac c
                           ,c'svm_parameter'nr_weight = 0
                           ,c'svm_parameter'weight_label = nullPtr
                           ,c'svm_parameter'weight = nullPtr
                           ,c'svm_parameter'nu = realToFrac nu
                           ,c'svm_parameter'p = 0.1
                           ,c'svm_parameter'shrinking = 1
                           ,c'svm_parameter'probability = 1}
     P.mapM_ (\label ->
                do labelPtr <-
                     newArray .
                     P.map (realToFrac .
                            (\x ->
                               if x == P.fromIntegral label
                                  then 1.0
                                  else 0.0)) $
                     labels
                   let problem =
                         C'svm_problem {c'svm_problem'l =
                                          fromIntegral . length $ features
                                       ,c'svm_problem'y = labelPtr
                                       ,c'svm_problem'x = featurePtr}
                   model <-
                     with problem
                          (\problem' ->
                             with params
                                  (\params' -> c'svm_train problem' params'))
                   modelName' <-
                     newCString (modelName P.++ "_" P.++ (show label))
                   c'svm_save_model modelName' model)
             [(round labelMin) .. (round labelMax)]

oneVsRestPredict
  :: String -> FilePath -> (Int,Int) -> Sink (Double,Ptr C'svm_node) (ResourceT IO) () 
oneVsRestPredict modelName output (labelMin, labelMax) = do
  models <-
    liftIO $
    P.mapM
      (\label -> do
         modelNameCS <- newCString (modelName P.++ "_" P.++ (show label))
         model <- c'svm_load_model modelNameCS
         return model)
      [labelMin .. labelMax]
  (correct, total) <- func models (0, 0)
  let percent = (fromIntegral correct) / (fromIntegral total) * 100
      str = show percent
  liftIO $ putStrLn str
  liftIO $ writeFile output str
  where
    func
      :: [Ptr C'svm_model]
      -> (Int, Int)
      -> Sink (Double, Ptr C'svm_node) (ResourceT IO) (Int, Int)
    func models (correct, total) = do
      x <- await
      case x of
        Nothing -> return (correct, total)
        Just (target, featurePtr) -> do
          probability <-
            liftIO $
            P.mapM
              (\model ->
                 allocaArray
                   2
                   (\buf -> do
                      c'svm_predict_probability model featurePtr buf
                      lPtr <- F.peek model
                      l <- F.peek . c'svm_model'label $ lPtr
                      p <- peekArray 2 buf
                      if l == 0
                        then return . realToFrac . P.last $ p
                        else return . realToFrac . P.head $ p))
              models
          let prediction = maxIndex probability
          -- liftIO $ putStrLn $ show (round target) P.++ " " P.++ show prediction
          if round target == prediction
            then func models (correct + 1, total + 1)
            else func models (correct, total + 1)
    maxIndex
      :: Ord a
      => [a] -> Int
    maxIndex = fst . L.maximumBy (comparing snd) . zip [labelMin .. labelMax]

readLabelFile :: FilePath -> IO [Double]
readLabelFile filePath =
  do bs <- readFile filePath
     return . P.map (\x -> read x :: Double) . lines $! bs

labelSource :: FilePath -> Source IO Double
labelSource filePath =
  do labels <- liftIO . readLabelFile $ filePath
     sourceList labels

getFeatureVecPtr :: [Double] -> IO (Ptr C'svm_node)
getFeatureVecPtr xs =
  newArray (pairs P.++
            [C'svm_node (-1)
                        0])
  where pairs =
          P.zipWith (\i x ->
                       C'svm_node (P.fromIntegral i)
                                  (realToFrac x))
                    [1 ..]
                    xs
        max = P.maximum xs


getPreComputedKernelFeatureVecPtr
  :: Double -> [Double] -> IO (Ptr C'svm_node)
getPreComputedKernelFeatureVecPtr idx xs =
  newArray (((C'svm_node 0
                         (realToFrac idx)) :
             pairs) P.++
            [C'svm_node (-1)
                        0])
  where pairs =
          V.toList $
          V.zipWith (\i x ->
                       C'svm_node (P.fromIntegral i)
                                  (realToFrac x))
                    (V.generate (P.length xs)
                                (\i -> i + 1)) 
                    (V.fromList $ xs)
        max = P.maximum xs
