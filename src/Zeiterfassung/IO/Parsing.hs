{-# LANGUAGE OverloadedStrings #-}

module Zeiterfassung.IO.Parsing
  ( initZed
  , Zeiterfassungsdaten(..)
  , RawData
  ) where

import Data.Time.LocalTime
import qualified Zeiterfassung.Aggregations as Aggregations
import Zeiterfassung.Data
import Zeiterfassung.IO.Common
import qualified Zeiterfassung.Zeit as Zeit


parseWords :: [String] -> (Maybe LocalTime, Maybe LocalTime)
parseWords [firstDateString, _, secondDateString] =
  let
    firstDate = stringToLocalTime firstDateString
    secondDate = stringToLocalTime secondDateString
  in (firstDate, secondDate)
parseWords _ = (Nothing, Nothing)


parseLine :: String -> (Maybe LocalTime, Maybe LocalTime)
parseLine = parseWords . words


parseFile :: FilePath -> IO [(Maybe LocalTime, Maybe LocalTime)]
parseFile path = do
  contentLines <- reverse . lines <$> readFile path
  return $ parseLine <$> contentLines




initZed :: IO Zeiterfassungsdaten
initZed = do
  rd <- parseFile constPath
  now <- Zeit.getCurrentLocalTime 
  return . Aggregations.diffMachine now $ Zeiterfassungsdaten
    { rawData = rd
    , rawDataWithDiff = []
    , hoursPerIntervall = []
    , hasActiveLog = False
    }

