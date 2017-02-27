module Application.Video.IO where

import           Codec.FFmpeg
import           Codec.Picture
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           Data.Conduit
import           Data.Conduit.List            as CL
import           Data.Maybe
import Control.Arrow

{-# INLINE getFrameSource #-}

getFrameSource :: FilePath -> IO (IO (Maybe DynamicImage), IO ())
getFrameSource filePath =
  fmap (first $ fmap (fmap ImageY8)) . imageReader $ File filePath

videoFrameSource :: FilePath -> Source (ResourceT IO) DynamicImage
videoFrameSource filePath = do
  liftIO initFFmpeg
  (frameSource,cleanup) <- liftIO $ getFrameSource filePath
  go frameSource cleanup
  where
     go source cl = do
      maybeFrame <- liftIO source
      case maybeFrame of
        Nothing -> liftIO cl
        Just frame -> do
          yield frame
          go source cl


plotFrame :: FilePath -> DynamicImage -> IO ()
plotFrame = savePngImage


