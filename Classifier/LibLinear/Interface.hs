{-# LANGUAGE CPP                        #-}
{-# LANGUAGE ForeignFunctionInterface   #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE PatternGuards              #-}
module Classifier.LibLinear.Interface where

import           Control.Monad.IO.Class                        (liftIO)
import           Data.Conduit
import           Data.Conduit.List                             as CL
import           Foreign                                       as F
import           Foreign.C
import           Foreign.Marshal.Array
import           Classifier.LibLinear.Bindings
import           Classifier.LibLinear.Solver
import           Classifier.LibLinear.Example
import           Prelude                                       as P
import           System.IO


data TrainParams = TrainParams
  { trainSolver          :: Solver
  , trainC               :: Double
  , trainNumExamples     :: Int
  , trainFeatureIndexMax :: Int
  , trainModel           :: String
  } deriving (Show)

train
  :: TrainParams -> [Double] -> [Ptr C'feature_node] -> IO ()
train (TrainParams solver c numExample maxIndex modelName) label feature =
  do labelPtr <- getLabelVecPtr label
     featurePtr <- newArray feature
     let p =
           C'problem {c'problem'l = fromIntegral numExample
                     ,c'problem'n = fromIntegral maxIndex
                     ,c'problem'y = labelPtr
                     ,c'problem'x = featurePtr
                     ,c'problem'bias = -1.0}
     model <-
       with p
            (\problem' ->
               with (newParameter solver c)
                    (\param' -> c'train problem' param'))
     modelName <- newCString modelName
     c'save_model modelName model

predict
  :: String -> FilePath -> Sink (Double,[C'feature_node]) IO ()
predict predictModel output = do
  modelName <- liftIO $ newCString predictModel
  model <- liftIO $ c'load_model modelName
  (correct, total) <- CL.foldM (func model) (0, 0)
  let percent = fromIntegral correct / fromIntegral total * 100
      str = show percent
  liftIO $ putStrLn str
  h <- liftIO $ openFile output WriteMode
  liftIO $ hPutStrLn h str
  liftIO $ hClose h
  where
    func :: Ptr C'model
         -> (Int, Int)
         -> (Double, [C'feature_node])
         -> IO (Int, Int)
    func model (correct, total) (target, xs) = do
      prediction <- withArray xs (c'predict model)
      let correctNew =
            if round target == round prediction
              then correct + 1
              else correct
      putStrLn $
        show target P.++ " " P.++ show prediction P.++ " " P.++
        show (fromIntegral correctNew / fromIntegral (total + 1) * 100) P.++
        "%"
      if round target == round prediction
        then return (correct + 1, total + 1)
        else return (correct, total + 1)

findParameterC
  :: TrainParams -> [Double] -> [Ptr C'feature_node] -> IO ()
findParameterC (TrainParams solver c numExample maxIndex modelName) label feature =
  do labelPtr <- getLabelVecPtr label
     featurePtr <- newArray feature
     let p =
           C'problem {c'problem'l = fromIntegral numExample
                     ,c'problem'n = fromIntegral maxIndex
                     ,c'problem'y = labelPtr
                     ,c'problem'x = featurePtr
                     ,c'problem'bias = -1.0}
     with p
          (\problem' ->
             with (newParameter solver c)
                  (\param' ->
                     allocaBytes
                       8
                       (\bestC' ->
                          allocaBytes
                            8
                            (\bestRate' ->
                               do c'find_parameter_C problem'
                                                     param'
                                                     3
                                                     (realToFrac c)
                                                     1024
                                                     bestC'
                                                     bestRate'
                                  bestC <- F.peek bestC'
                                  bestRate <- F.peek bestRate'
                                  putStrLn $
                                    "Best C = " ++
                                    show bestC ++
                                    " CV accuracy = " ++
                                    show (100 * bestRate) ++ "%"))))
