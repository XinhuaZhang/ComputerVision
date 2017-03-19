module Application.HandWriting.IO
  ( module Application.HandWriting.Types
  , hwdbSource
  , offlineCharacterConduit
  ) where

import           Application.HandWriting.Types
import           Control.Monad                 as M
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           Data.Binary.Get
import           Data.ByteString               as BS
import           Data.ByteString.Lazy          as BL
import           Data.Conduit
import           Data.Conduit.Binary           as CB
import           Data.List                     as L
import           Data.Vector.Unboxed           as VU
import           System.IO                     as IO

offlineCharacterConduit :: Conduit BS.ByteString (ResourceT IO) OfflineCharacter
offlineCharacterConduit = do
  sizeBs <- CB.take 4
  unless
    (BL.null sizeBs)
    (do let size = fromIntegral . runGet getWord32le $ sizeBs
        dataBs <- CB.take size
        yield . runGet getOfflineCharacter $ dataBs
        offlineCharacterConduit)

{-# INLINE getOfflineCharacter #-}

getOfflineCharacter :: Get OfflineCharacter
getOfflineCharacter = do
  t <- getInt16le
  w <- getWord16le
  h <- getWord16le
  bm <- M.replicateM (fromIntegral $ w * h) getWord8
  return $! OfflineCharacter t w h . VU.fromListN (fromIntegral $ w * h) $ bm

hwdbSource :: FilePath -> Source (ResourceT IO) BS.ByteString
hwdbSource filePath = do
  pathList <- liftIO . fmap L.lines . IO.readFile $ filePath
  M.mapM_ CB.sourceFile pathList
