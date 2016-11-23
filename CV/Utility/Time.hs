module CV.Utility.Time where


import Data.Time

printCurrentTime :: IO ()
printCurrentTime = do
  time <- getZonedTime
  print . localTimeOfDay . zonedTimeToLocalTime $ time
  

getCurrentTimeInt :: IO Int
getCurrentTimeInt = do
  time <- getCurrentTime
  return . fromIntegral . diffTimeToPicoseconds . utctDayTime $ time
