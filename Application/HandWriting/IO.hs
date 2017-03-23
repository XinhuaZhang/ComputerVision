module Application.HandWriting.IO
  ( module Application.HandWriting.Types
  , hwdbSource
  , offlineCharacterConduit
  , writeSink
  , sparseOfflineCharacterConduit
  ) where

import           Application.HandWriting.Types
import           Control.Monad                 as M
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           CV.Utility.Time
import           Data.Binary
import           Data.Binary.Get
import           Data.ByteString               as BS
import           Data.ByteString.Lazy          as BL
import           Data.Conduit
import           Data.Conduit.Binary           as CB
import           Data.Conduit.List             as CL
import           Data.IntMap.Strict            as IM
import           Data.List                     as L
import           Data.Vector.Unboxed           as VU
import           Data.Word
import           System.IO                     as IO

offlineCharacterConduit :: IntMap Word16 -> Conduit BS.ByteString (ResourceT IO) OfflineCharacter
offlineCharacterConduit intMap = do
  sizeBs <- CB.take 4
  unless
    (BL.null sizeBs)
    (do let size' = fromIntegral . runGet getWord32le $ sizeBs
        dataBs <- CB.take (size' - 4)
        yield . runGet (getOfflineCharacter intMap) $ dataBs
        offlineCharacterConduit intMap)

{-# INLINE getOfflineCharacter #-}

getOfflineCharacter :: IntMap Word16 -> Get OfflineCharacter
getOfflineCharacter intMap = do
  t <- getWord16le
  w <- getWord16le
  h <- getWord16le
  bm <- M.replicateM (fromIntegral $ w * h) getWord8
  let label' = intMap IM.! fromIntegral t
  return $!
    OfflineCharacter label' w h .
    VU.map (\x -> 255 - x) . VU.fromListN (fromIntegral $ w * h) $
    bm

hwdbSource :: FilePath -> Source (ResourceT IO) BS.ByteString
hwdbSource filePath = do
  pathList <- liftIO . fmap L.lines . IO.readFile $ filePath
  M.mapM_
    (\path -> do
       liftIO printCurrentTime
       CB.sourceFile path)
    pathList


writeSink :: FilePath -> Sink BL.ByteString (ResourceT IO) ()
writeSink filePath = do
  h <- liftIO $ openBinaryFile filePath WriteMode
  CL.foldMapM
    (\x ->
        liftIO $
        do let len' = fromIntegral . BL.length $ x :: Word32
           BL.hPut h . encode $ len'
           BL.hPut h x)
  liftIO $ hClose h
  
sparseOfflineCharacterConduit :: Conduit BS.ByteString (ResourceT IO) SparseOfflineCharacter
sparseOfflineCharacterConduit = do
  sizeBs <- CB.take 4
  unless
    (BL.null sizeBs)
    (do let size' = fromIntegral (decode sizeBs :: Word32) 
        dataBs <- CB.take size'
        yield . decode $ dataBs
        sparseOfflineCharacterConduit)
