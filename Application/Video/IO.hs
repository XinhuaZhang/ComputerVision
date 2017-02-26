module Application.Video.IO where

import           Codec.FFmpeg
import           Codec.Picture
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           Data.Conduit
import           Data.Conduit.List            as CL
import           Data.Maybe

{-# INLINE getFrameSource #-}

getFrameSource :: FilePath -> IO (IO (Maybe DynamicImage), IO ())
getFrameSource filePath = do
  (getFrame, cleanup) <- imageReader $ File filePath
  return (fmap ImageRGB8 <$> getFrame , cleanup)


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


