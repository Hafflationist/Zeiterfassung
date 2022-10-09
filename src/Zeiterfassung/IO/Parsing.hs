{-# LANGUAGE OverloadedStrings #-}

module Zeiterfassung.IO.Parsing
  ( initZed
  , Zeiterfassungsdaten(..)
  , RawData
  ) where

import Data.Attoparsec.Text
import Data.Attoparsec.Time
import Data.DateTime
import qualified Data.Time as Time
import qualified Data.Time.Clock as Clock
import Data.Time.LocalTime
import qualified Data.Time.LocalTime as LocalTime
import qualified Data.Text as Txt
import qualified Data.Maybe as Maybe
import Zeiterfassung.Data
import qualified Zeiterfassung.Zeit as Zeit


constPath :: FilePath
constPath = "/Schnitt/Uni/MA/malog.txt"


parseWords :: LocalTime -> [String] -> Maybe (DateTime, DateTime)
parseWords now [firstDateString, _, secondDateString] =
  let 
    firstDate = parseOnly utcTime  (Txt.pack (firstDateString ++ ":00Z"))
    secondDate = parseOnly utcTime  (Txt.pack (secondDateString ++ ":00Z"))
    getResult (Right a) (Right b) = Just (a, b)
    getResult (Right a) _ = Just (a, LocalTime.localTimeToUTC LocalTime.utc now)
    getResult _ _ = Nothing
  in getResult firstDate secondDate
parseWords _ _ = Nothing


parseLine :: LocalTime -> String -> Maybe (DateTime, DateTime)
parseLine now = parseWords now . words


parseFile :: FilePath -> IO [(DateTime, DateTime)]
parseFile path = do
  now <- Zeit.getCurrentLocalTime
  contentLines <- reverse . lines <$> readFile path
  return $ Maybe.catMaybes $ parseLine now <$> contentLines


diffMachine :: [(DateTime, DateTime)] -> RawData
diffMachine = fmap (\ (from, to) -> (from, to, DTD . Clock.diffUTCTime to $ from))


initZed :: IO Zeiterfassungsdaten
initZed = do
  preRd <- parseFile constPath
  let rd = diffMachine preRd
  return Zeiterfassungsdaten
    { rawData = rd
    , hoursPerIntervall = []
    , hasActiveLog = False
    }
