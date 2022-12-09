{-# LANGUAGE OverloadedStrings #-}

module Zeiterfassung.IO.Parsing
  ( initZed
  , Zeiterfassungsdaten(..)
  , RawData
  ) where

import Data.DateTime
import qualified Zeiterfassung.Aggregations as Aggregations
import Zeiterfassung.Data
import Zeiterfassung.IO.Common
import qualified Zeiterfassung.Zeit as Zeit


parseWords :: [String] -> (Maybe DateTime, Maybe DateTime)
parseWords [firstDateString, _, secondDateString] =
  let
    firstDate = stringToDateTime firstDateString
    secondDate = stringToDateTime secondDateString
  in (firstDate, secondDate)
parseWords _ = (Nothing, Nothing)


parseLine :: String -> (Maybe DateTime, Maybe DateTime)
parseLine = parseWords . words


parseFile :: FilePath -> IO [(Maybe DateTime, Maybe DateTime)]
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