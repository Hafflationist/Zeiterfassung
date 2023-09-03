module Zeiterfassung.Aggregations
  (diffMachine, sumAllHours, weeklyHours, averageHoursPerWeek) where

import qualified Data.List as List ( map, sum, groupBy, filter )
import qualified Data.Maybe as Maybe
import Data.Time (UTCTime())
import qualified Data.Time.Clock as Clock
import Data.Time.LocalTime
import qualified Data.Time.LocalTime as LocalTime
import Data.Time.Calendar.WeekDate
import Prelude
import Zeiterfassung.Data 



diff :: UTCTime -> Maybe LocalTime -> Maybe LocalTime -> DateTimeDiff 
diff _ (Just from) (Just to) = 
    let
      utcFrom = LocalTime.localTimeToUTC LocalTime.utc from
      utcTo = LocalTime.localTimeToUTC LocalTime.utc to
    in DTD . Clock.diffUTCTime utcTo $ utcFrom 
diff now (Just from) Nothing =
    let
      utcFrom = LocalTime.localTimeToUTC LocalTime.utc from
    in DTD . Clock.diffUTCTime now $ utcFrom
diff now _ _ = DTD . Clock.diffUTCTime now $ now


diffMachine :: LocalTime -> Zeiterfassungsdaten -> Zeiterfassungsdaten 
diffMachine now zed = 
  let 
    localDtm = LocalTime.localTimeToUTC LocalTime.utc now
    newRawDataWithDiff = fmap (\ (from, to) -> (from, to, diff localDtm from to)) . rawData $ zed
  in zed {
    rawDataWithDiff  = newRawDataWithDiff
  }


sumAllHours :: RawDataWithDiff -> Double
sumAllHours rdwd = (List.sum . List.map (\(_,_, DTD d) -> fromInteger . round $ d) $ rdwd) / 3600.0


getWeek :: Maybe LocalTime -> Int
getWeek Nothing = -2
getWeek (Just dt) =
  let
    (_, weekOfYear, _) = toWeekDate . localDay $ dt
  in weekOfYear


weeklyHours :: RawDataWithDiff -> [(Int, DateTimeDiff)]
weeklyHours rdwd =
  let
    grouper (week1, _) (week2, _) = week1 == week2
    sumPerWeek :: [(Int, DateTimeDiff)] -> Maybe (Int, DateTimeDiff)
    sumPerWeek [] = Nothing
    sumPerWeek weekData@(firstWeekEntry:_) =
      let 
        (weekNumber, _) = firstWeekEntry
        weekHoursSum = List.sum . List.map snd $ weekData
      in Just (weekNumber, weekHoursSum)
  in Maybe.mapMaybe sumPerWeek
   . List.groupBy grouper 
   . List.map (\ (von, _, d) -> (getWeek von, d)) 
   $ rdwd


averageHoursPerWeek :: RawDataWithDiff -> Double
averageHoursPerWeek rdwd =
  let
    weeklyHoursList = List.filter (\ (num, _) -> num >= 0) . weeklyHours $ rdwd 
    numberOfWeek = Prelude.length weeklyHoursList
    DTD ndt = List.sum (snd <$> weeklyHoursList)
    hours = (fromInteger . round $ ndt) / 3600.0
  in hours / fromIntegral numberOfWeek
