module Zeiterfassung.Zeit (getCurrentLocalTime) where

import qualified Data.Time as Time
import Data.Time.LocalTime


getCurrentLocalTime :: IO LocalTime
getCurrentLocalTime = do
  now <- Time.getCurrentTime
  timezone <- getCurrentTimeZone
  return . Time.utcToLocalTime timezone $ now