module Zeiterfassung.Aggregations
  (sumAllHours, weeklyHours, averageHoursPerWeek) where

import Data.DateTime (DateTime)
import qualified Data.List as List ( map, sum, groupBy )
import qualified Data.Maybe as Maybe
import Data.Time (UTCTime(utctDay))
import Data.Time.Calendar.WeekDate
import Prelude
import Zeiterfassung.Data (Zeiterfassungsdaten (..), DateTimeDiff (..))



sumAllHours :: Zeiterfassungsdaten -> Double
sumAllHours zed = (List.sum . List.map (\(_,_, DTD d) -> fromInteger . round $ d) . rawData $ zed) / 3600.0


getWeek :: Maybe DateTime -> Int
getWeek Nothing = -2
getWeek (Just dt) =
  let
    (_, weekOfYear, _) = toWeekDate . utctDay $ dt
  in weekOfYear


weeklyHours :: Zeiterfassungsdaten -> [(Int, DateTimeDiff)]
weeklyHours zed =
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
   . List.map (\ (von, _, diff) -> (getWeek von, diff)) 
   . rawData $ zed


averageHoursPerWeek :: Zeiterfassungsdaten ->  Double
averageHoursPerWeek zed =
  let
    hours = sumAllHours zed
    weeklyHoursList = weeklyHours zed
    numberOfWeek = Prelude.length weeklyHoursList
  in hours / fromIntegral numberOfWeek