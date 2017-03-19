{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE ForeignFunctionInterface   #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE PatternGuards              #-}
module Classifier.LibLinear.Interface where

import           Classifier.LibLinear.Bindings
import           Classifier.LibLinear.Example
import           Classifier.LibLinear.Solver
import           Control.Monad                 as M
import           Control.Monad.IO.Class        (liftIO)
import           Control.Monad.Trans.Resource
import           Data.Conduit
import           Data.Conduit.List             as CL
import           Data.List                     as L
import           Foreign                       as F
import           Foreign.C
import           Foreign.Marshal.Array
import           Prelude                       as P
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
  do when (P.length feature /= numExample)
          (putStrLn $
           "train: numExample (" P.++ show numExample P.++
           ") in train parameters doesn't equal to the actual number (" P.++
           show (P.length feature) P.++
           ")")
     labelPtr <- getLabelVecPtr label
     featurePtr <- newArray feature
     let p =
           C'problem {c'problem'l = fromIntegral . P.length $ feature
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
  :: String -> FilePath -> Sink (Double,[C'feature_node]) (ResourceT IO) ()
predict predictModel output = do
  modelName <- liftIO $ newCString predictModel
  model <- liftIO $ c'load_model modelName
  (correct, total) <- func model (0, 0)
  let percent = fromIntegral correct / (fromIntegral total :: Double) * 100
      str = show percent
  liftIO $ putStrLn str
  h <- liftIO $ openFile output WriteMode
  liftIO $ hPutStrLn h str
  liftIO $ hClose h
  where
    func
      :: Ptr C'model
      -> (Int, Int)
      -> Sink (Double, [C'feature_node]) (ResourceT IO) (Int, Int)
    func model (!correct, !total) = do
      x <- await
      case x of
        Just (!target, !xs) -> do
          prediction <- liftIO $ withArray xs (c'predict model)
          let !correctNew =
                if (round target :: Int) == round prediction
                  then correct + 1
                  else correct
          -- liftIO $
          --   putStrLn $
          --   show target P.++ " " P.++ show prediction P.++ " " P.++
          --   show
          --     (fromIntegral correctNew / fromIntegral (total + 1) * 100 :: Double) P.++
          --   "%"
          if (round target :: Int) == round prediction
            then func model (correct + 1, total + 1)
            else func model (correct, total + 1)
        Nothing -> return (correct, total)

findParameterC
  :: TrainParams -> [Double] -> [Ptr C'feature_node] -> IO Double
findParameterC (TrainParams solver c numExample maxIndex modelName) label feature =
  do when (P.length feature /= numExample)
          (putStrLn $
           "findParameterC: numExample (" P.++ show numExample P.++
           ") in train parameters doesn't equal to the actual number (" P.++
           show (P.length feature) P.++
           ")")
     labelPtr <- getLabelVecPtr label
     featurePtr <- newArray feature
     let p =
           C'problem {c'problem'l = fromIntegral . P.length $ feature
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
                                                     5
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
                                    show (100 * bestRate) ++ "%"
                                  return . (\(CDouble x) -> x) $ bestC))))

trainNPredict
  :: TrainParams
  -> [(Double, [C'feature_node])]
  -> [(Double, [C'feature_node])]
  -> IO Double
trainNPredict (TrainParams solver c numExample maxIndex _modelName) trainLabelFeature testLabelFeature = do
  when
    (numExample /= P.length trainLabelFeature)
    (error $
     "trainNPredict: numExample (" P.++ show numExample P.++
     ") in train parameters doesn't equal to the actual number (" P.++
     show (P.length trainLabelFeature) P.++
     ")")
  when
    ((maxIndex + 1) /= (P.length . snd . P.head $ trainLabelFeature))
    (error $
     "trainNPredict: maxIndex (" P.++ show maxIndex P.++
     ") in train parameters doesn't equal to the actual number (" P.++
     show (P.length . snd . P.head $ trainLabelFeature) P.++
     ")")
  trainLabelPtr <- getLabelVecPtr . fst . L.unzip $ trainLabelFeature
  trainFeaturePtr <- join . fmap newArray . M.mapM newArray . snd . L.unzip $ trainLabelFeature
  let p =
        C'problem
        { c'problem'l = fromIntegral . P.length $ trainLabelFeature
        , c'problem'n = fromIntegral maxIndex
        , c'problem'y = trainLabelPtr
        , c'problem'x = trainFeaturePtr
        , c'problem'bias = -1.0
        }
  model <- with p (with (newParameter solver c) . c'train)
  (numCorrect, numTotal) <-
    M.foldM
      (\(!correct, !total) (target, xs) -> do
         prediction <- withArray xs (c'predict model)
         let correctNew =
               if (round target :: Int) == round prediction
                 then correct + 1
                 else correct
         -- putStrLn $
         --   show target P.++ " " P.++ show prediction P.++ " " P.++
         --   show (correctNew / (total + 1) * 100) P.++
         --   "%"
         return $
           if (round target :: Int) == round prediction
             then (correct + 1, total + 1)
             else (correct, total + 1))
      (0 :: Double, 0 :: Double)
      testLabelFeature
  putStrLn $ show (numCorrect / numTotal * 100) P.++ "%"
  return $! numCorrect / numTotal
