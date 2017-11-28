{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE ScopedTypeVariables      #-}
module Application.CaffeData.Caffe
  ( saveDataSink
  )
where

import           Application.CaffeData.Bindings
import           Control.Monad                  as M
import           Control.Monad.IO.Class         (liftIO)
import           Control.Monad.Trans.Resource
import           CV.Array.LabeledArray
import           Data.Array.Repa                as R
import           Data.Char
import           Data.Conduit
import           Data.Conduit.List              as CL
import           Data.List                      as L
import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.Marshal.Array
import           Foreign.Ptr
import           Text.Printf

{-# INLINE createLabelVec #-}

createLabelVec :: [Int] -> IO (Ptr CInt)
createLabelVec label = newArray $ L.map fromIntegral label

{-# INLINE createDataArr #-}

createDataArr :: [[Double]] -> IO [Ptr CUChar]
createDataArr = M.mapM (newArray . L.map (castCharToCUChar . chr . round))

{-# INLINE saveData #-}

saveData :: Int -> Int -> Sink (LabeledArray DIM3 Double) (ResourceT IO) ()
saveData batchSize n = do
  batch <- CL.take batchSize
  if L.null batch
    then liftIO $ c'closeDatabase
    else let (Z :. channels :. rows :. cols) =
               extent . (\(LabeledArray _ arr) -> arr) . L.head $ batch
             (label, feature) =
               L.unzip .
               L.map (\(LabeledArray label vec) -> (label, R.toList vec)) $
               batch
         in do labelVec <- liftIO $ createLabelVec label
               featuresVec <- liftIO $ createDataArr feature
               liftIO $
                 withArray
                   featuresVec
                   (\featureIndexVec ->
                      c'saveData
                        (fromIntegral cols)
                        (fromIntegral rows)
                        (fromIntegral channels)
                        (fromIntegral $ length batch)
                        (fromIntegral $ n * batchSize)
                        featureIndexVec
                        labelVec)
               saveData batchSize (n + 1)

saveDataSink :: String
             -> Int
             -> Sink (LabeledArray DIM3 Double) (ResourceT IO) ()
saveDataSink path batchSize = do
  pathCSString <- liftIO $ newCString path
  typeCSString <- liftIO $ newCString "lmdb"
  liftIO $ c'openDatabase typeCSString pathCSString
  saveData batchSize 0
