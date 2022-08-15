{-# LANGUAGE OverloadedStrings #-}

module Zeiterfassung
  ( initGame
  , Zeiterfassungsdaten(..)
  , RawData
  ) where

import Data.Attoparsec.Text
import Data.Attoparsec.Time
import Data.DateTime
import qualified Data.Text as Txt
import qualified Data.Maybe as Maybe

-- Types

data Zeiterfassungsdaten = Zeiterfassungsdaten
  { rawData           :: RawData
  , workedHours       :: Double
  , hoursPerIntervall :: IntervallData
  , hasActiveLog      :: Bool
  } deriving (Show, Eq)

type RawData = [(DateTime, DateTime)]

type IntervallData = [(String, Int)]


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


parseFile :: FilePath -> IO RawData
parseFile path = do
  contentLines <- reverse . lines <$> readFile path
  return $ Maybe.catMaybes $ parseLine <$> contentLines


initGame :: IO Zeiterfassungsdaten
initGame = do
  currentRawData <- parseFile constPath
  print currentRawData
  return Zeiterfassungsdaten
    { rawData = currentRawData
    , workedHours = 0.0
    , hoursPerIntervall = []
    , hasActiveLog = False
    }
