{-# LANGUAGE OverloadedStrings #-}

module Zeiterfassung.IO.Parsing
  ( initZed
  , Zeiterfassungsdaten(..)
  , RawData
  ) where

import Data.Attoparsec.Text
import Data.Attoparsec.Time
import Data.DateTime
import qualified Data.Either.Extra as EitherExtra
import qualified Data.Text as Txt
import qualified Zeiterfassung.Aggregations as Aggregations
import Zeiterfassung.Data
import qualified Zeiterfassung.Zeit as Zeit


parseWords :: [String] -> (Maybe DateTime, Maybe DateTime)
parseWords [firstDateString, _, secondDateString] =
  let 
    firstDate = parseOnly utcTime  (Txt.pack (firstDateString ++ ":00Z"))
    secondDate = parseOnly utcTime  (Txt.pack (secondDateString ++ ":00Z"))
  in (EitherExtra.eitherToMaybe firstDate, EitherExtra.eitherToMaybe secondDate)
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