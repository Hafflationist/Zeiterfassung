{-# LANGUAGE OverloadedStrings #-}

module Zeiterfassung.Parsing
  ( initZed
  , Zeiterfassungsdaten(..)
  , RawData
  ) where

import Data.Attoparsec.Text
import Data.Attoparsec.Time
import Data.DateTime
import qualified Data.Time.Clock as Clock
import qualified Data.Text as Txt
import qualified Data.Maybe as Maybe
import Zeiterfassung.Data
import Data.Time (NominalDiffTime)


constPath :: FilePath
constPath = "/Schnitt/Uni/MA/malog.txt"


parseWords :: [String] -> Maybe (DateTime, DateTime)
parseWords [firstDateString, _, secondDateString] =
  let 
    firstDate = parseOnly utcTime  (Txt.pack (firstDateString ++ ":00Z"))
    secondDate = parseOnly utcTime  (Txt.pack (secondDateString ++ ":00Z"))
    getResult (Right a) (Right b) = Just (a, b)
    getResult _ _ = Nothing
  in getResult firstDate secondDate
parseWords _ = Nothing


parseLine :: String -> Maybe (DateTime, DateTime)
parseLine = parseWords . words


parseFile :: FilePath -> IO [(DateTime, DateTime)]
parseFile path = do
  contentLines <- reverse . lines <$> readFile path
  return $ Maybe.catMaybes $ parseLine <$> contentLines


diffMachine :: [(DateTime, DateTime)] -> RawData
diffMachine = fmap (\ (from, to) -> (from, to, Clock.diffUTCTime from to))


initZed :: IO Zeiterfassungsdaten
initZed = do
  preRd <- parseFile constPath
  let rd = diffMachine preRd
  return Zeiterfassungsdaten
    { rawData = rd
    , workedHours = 0.0
    , hoursPerIntervall = []
    , hasActiveLog = False
    }
