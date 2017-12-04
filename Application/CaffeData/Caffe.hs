{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE ScopedTypeVariables      #-}
module Application.CaffeData.Caffe
  ( saveData,
    saveDataSink
  )
where

import           Application.CaffeData.Bindings
import           Control.Monad                  as M
import           Control.Monad.IO.Class         (liftIO)
import           Control.Monad.Trans.Resource
import           CV.Array.LabeledArray
import           CV.IO.ImageIO
import           Data.Array.Repa                as R
import           Data.Char
import           Data.Conduit
import           Data.Conduit.List              as CL
import           Data.List                      as L
import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.Marshal.Array
import           Foreign.Ptr
import           System.Random
import           Text.Printf

{-# INLINE createLabelVec #-}

createLabelVec :: [Int] -> IO (Ptr CInt)
createLabelVec label = newArray $ L.map fromIntegral label

{-# INLINE createDataArr #-}

createDataArr :: [[Double]] -> IO [Ptr CUChar]
createDataArr = M.mapM (newArray . L.map (castCharToCUChar . chr . round))

{-# INLINE saveData #-}

saveData :: Int -> Int -> Sink [LabeledArray DIM3 Double] (ResourceT IO) ()
saveData batchSize n = do
  batchList <- CL.take batchSize
  rs <- liftIO $ (M.replicateM (L.length batchList) randomIO :: IO [Int])
  if L.null batchList
    then liftIO $ c'closeDatabase
    else do
      liftIO $
        M.zipWithM_
          (\i batch ->
             let (Z :. channels :. rows :. cols) =
                   extent . (\(LabeledArray _ arr) -> arr) . L.head $ batch
                 shuffledBatch = snd . L.unzip . L.sortOn fst . L.zip rs $ batch
                 (label, feature) =
                   L.unzip .
                   L.map
                     (\(LabeledArray label vec) ->
                        (label, normalize (0, 255) . R.toList $ vec)) $
                   shuffledBatch
             in do labelVec <- createLabelVec label
                   featuresVec <- createDataArr feature
                   withArray
                     featuresVec
                     (\featureIndexVec ->
                        c'saveData
                          (fromIntegral i)
                          (fromIntegral cols)
                          (fromIntegral rows)
                          (fromIntegral channels)
                          (fromIntegral $ length batch)
                          (fromIntegral $ n * batchSize)
                          featureIndexVec
                          labelVec))
          [(0 :: Int) ..] .
        L.transpose $
        batchList
      saveData batchSize (n + 1)

saveDataSink :: String
             -> Int
             -> Sink [LabeledArray DIM3 Double] (ResourceT IO) ()
saveDataSink path batchSize = do
  x <- CL.peek
  case x of
    Nothing -> return ()
    Just y -> do
      pathCSString <- liftIO $ newCString path
      typeCSString <- liftIO $ newCString "lmdb"
      liftIO $
        c'openDatabase typeCSString pathCSString (fromIntegral . L.length $ y)
      saveData batchSize 0

{-# INLINE normalize #-}

normalize :: (Double,Double) -> [Double] -> [Double]
normalize (lb, ub) xs
  | L.any isNaN xs = error $ "normalize: NaN\n" L.++ show xs
  | L.all (== 0) xs = xs
  | otherwise = L.map (\x -> (x - minV) / (maxV - minV) * (ub - lb) + lb) xs
  where
    minV = L.minimum xs
    maxV = L.maximum xs
