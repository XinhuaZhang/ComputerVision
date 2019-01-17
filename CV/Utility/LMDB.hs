{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE ScopedTypeVariables      #-}
module CV.Utility.LMDB
  ( saveData,
    saveDataSink,
    saveFloatData,
    saveFloatDataSink
  )
where

import           Control.Monad                as M
import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Trans.Resource
import           CV.Utility.LMDB.Bindings
import           Data.Array.Repa              as R
import           Data.Char
import           Data.Conduit
import           Data.Conduit.List            as CL
import           Data.List                    as L
import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.Marshal.Array
import           Foreign.Ptr
import           GHC.Float
import           System.Directory
import           System.FilePath
import           System.Random
import           Text.Printf

{-# INLINE createLabelVec #-}

createLabelVec :: [Int] -> IO (Ptr CInt)
createLabelVec label = newArray $ L.map fromIntegral label

{-# INLINE createDataArr #-}

createDataArr :: [[Double]] -> IO [Ptr CUChar]
createDataArr = M.mapM (newArray . L.map (castCharToCUChar . chr . round))

{-# INLINE createFloatDataArr #-}

createFloatDataArr :: [[Double]] -> IO [Ptr CFloat]
createFloatDataArr = M.mapM (newArray . L.map (CFloat . double2Float))

{-# INLINE saveData #-}

saveData :: Int
         -> Int
         -> Sink [(Int, R.Array U DIM3 Double)] (ResourceT IO) ()
saveData batchSize n = do
  batchList <- CL.take batchSize
  rs <- liftIO $ (M.replicateM (L.length batchList) randomIO :: IO [Int])
  if L.null batchList
    then liftIO $ c'closeDatabase
    else do
      liftIO $
        M.zipWithM_
          (\i batch ->
             let (Z :. channels :. rows :. cols) = extent . snd . L.head $ batch
                 shuffledBatch = snd . L.unzip . L.sortOn fst . L.zip rs $ batch
                 (label, feature) =
                   L.unzip .
                   L.map
                     (\(label, vec) ->
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

{-# INLINE saveFloatData #-}

saveFloatData :: Int
              -> Int
              -- -> ConduitT (Double, [R.Array U DIM3 Double]) Void (ResourceT IO) ()
              -> Sink (Double, [R.Array U DIM3 Double]) (ResourceT IO) ()
saveFloatData batchSize n = do
  batchList <- CL.take batchSize
  if L.null batchList
    then liftIO $ c'closeDatabase
    else do
      let (labels, xs) = L.unzip batchList
      liftIO .
        M.zipWithM_
          (\i batch ->
             let (Z :. channels :. rows :. cols) = extent . L.head $ batch
             in do labelVec <- createLabelVec . L.map round $ labels
                   featuresVec <- createFloatDataArr . L.map ( R.toList) $ batch
                   withArray
                     featuresVec
                     (\featureIndexVec ->
                        c'saveFloatData
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
        xs
      saveFloatData batchSize (n + 1)

saveDataSink :: String
             -> Int
             -> Sink [(Int, R.Array U DIM3 Double)] (ResourceT IO) ()
saveDataSink path batchSize = do
  x <- CL.peek
  case x of
    Nothing -> return ()
    Just y -> do
      let numLayer = L.length y
      liftIO $ createDirectoryIfMissing True (takeDirectory path)
      liftIO $
        M.mapM_
          (\i -> do
             let p = path L.++ "_" L.++ show i
             removePathForcibly p)
          [0 .. numLayer - 1]
      pathCSString <- liftIO $ newCString path
      typeCSString <- liftIO $ newCString "lmdb"
      liftIO $
        c'openDatabase typeCSString pathCSString (fromIntegral . L.length $ y)
      saveData batchSize 0

saveFloatDataSink
  :: String
  -> Int
  -- -> ConduitT (Double, [R.Array U DIM3 Double]) Void (ResourceT IO) ()
  -> Sink (Double, [R.Array U DIM3 Double]) (ResourceT IO) ()
saveFloatDataSink path batchSize = do
  x <- CL.peek
  case x of
    Nothing -> return ()
    Just y -> do
      let numLayer = L.length . snd $ y
      liftIO . M.mapM_ (print . extent) . snd $ y
      liftIO $ createDirectoryIfMissing True (takeDirectory path)
      liftIO $
        M.mapM_
          (\i -> do
             let p = path L.++ "_" L.++ show i
             removePathForcibly p)
          [0 .. numLayer - 1]
      pathCSString <- liftIO $ newCString path
      typeCSString <- liftIO $ newCString "lmdb"
      liftIO $ c'openDatabase typeCSString pathCSString (fromIntegral numLayer)
      saveFloatData batchSize 0

{-# INLINE normalize #-}

normalize :: (Double,Double) -> [Double] -> [Double]
normalize (lb, ub) xs
  | L.any isNaN xs = error $ "normalize: NaN\n" L.++ show xs
  | L.all (== 0) xs = xs
  | otherwise = L.map (\x -> (x - minV) / (maxV - minV) * (ub - lb) + lb) xs
  where
    minV = L.minimum xs
    maxV = L.maximum xs

