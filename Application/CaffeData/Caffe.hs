{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE ScopedTypeVariables      #-}
module Application.CaffeData.Caffe
  -- ( saveDataSink
  -- , FeatureInfo(..)
  -- , minMaxSink
  -- )
where

import           Application.CaffeData.Bindings
import           Control.Monad                  as M
import           Control.Monad.IO.Class         (liftIO)
import           CV.Array.LabeledArray
import           Data.Char
import           Data.Conduit
import           Data.Conduit.List              as CL
import           Data.List                      as L
import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.Marshal.Array
import           Foreign.Ptr

data FeatureInfo = FeatureInfo
  { width   :: Int
  , height  :: Int
  , channel :: Int
  } deriving (Show)


createLabelVec :: [Int] -> IO (Ptr CInt)
createLabelVec label = newArray $ L.map fromIntegral label

createDataArr :: [[Double]] -> IO [Ptr CUChar]
createDataArr = M.mapM (newArray . L.map (castCharToCUChar . chr . round)) 


saveData :: Int
         -> Int
         -> FeatureInfo
         -> Sink (Int, [Double]) IO ()
saveData batchSize n fi@FeatureInfo {width = w
                                    ,height = h
                                    ,channel = c} = do
  batch <- CL.take batchSize
  if not (null batch)
    then let (label, feature) = unzip batch
         in if (div (sum (L.map length feature)) (length feature)) /=
               (w * h * c)
              then do
                liftIO $ print $ (L.length . L.head) feature
                liftIO $ print (w * h * c)
                error "Dimension error"
              else do
                labelVec <- liftIO $ createLabelVec label
                featuresVec <- liftIO $ createDataArr feature
                liftIO $
                  withArray
                    featuresVec
                    (\featureIndexVec ->
                        c'saveData
                          (fromIntegral w)
                          (fromIntegral h)
                          (fromIntegral c)
                          (fromIntegral $ length batch)
                          (fromIntegral $ n * (length batch))
                          featureIndexVec
                          labelVec)
                saveData batchSize (n + 1) fi
    else do
      liftIO $ c'closeDatabase
      return ()

-- saveDataSink :: String
--              -> Int
--              -> FeatureInfo
--              -> Sink (Int,[Double]) IO ()
-- saveDataSink path batchSize fi =
--   do pathCSString <- liftIO $ newCString path
--      typeCSString <- liftIO $ newCString "lmdb"
--      liftIO $ c'openDatabase typeCSString pathCSString
--      saveData batchSize 0 fi 
