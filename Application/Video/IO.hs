--module Application.Video.IO where

import Codec.FFmpeg
import Codec.Picture
import Data.Maybe


go :: FilePath -> IO (Maybe DynamicImage)
go filePath = do
  (getFrame, cleanup) <- imageReader $ File filePath
  (fmap ImageRGB8 <$> getFrame) <* cleanup

main = do
  maybeFrame <-
    go
      "/Users/xzhang/WorkSpace/ComputerVision/Application/Video/v_Bowling_g01_c01.avi"
  case maybeFrame of
    Nothing -> error "error reading frame"
    Just (ImageRGB8 frame) -> print . imageWidth $ frame
