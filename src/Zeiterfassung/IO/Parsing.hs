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
import qualified Data.Time.Clock as Clock
import Data.Time.LocalTime
import qualified Data.Time.LocalTime as LocalTime
import qualified Data.Text as Txt
import Zeiterfassung.Data
import qualified Zeiterfassung.Zeit as Zeit


constPath :: FilePath
constPath = "/Schnitt/Uni/MA/malog.txt"


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


diff :: DateTime -> Maybe DateTime -> Maybe DateTime -> DateTimeDiff
diff _ (Just from) (Just to) = DTD . Clock.diffUTCTime to $ from
diff now (Just from) Nothing = DTD . Clock.diffUTCTime now $ from
diff now _ _ = DTD . Clock.diffUTCTime now $ now


diffMachine :: LocalTime -> [(Maybe DateTime, Maybe DateTime)] -> RawData
diffMachine now rawRawData = 
  let 
    localDtm = LocalTime.localTimeToUTC LocalTime.utc now
  in fmap (\ (from, to) -> (from, to, diff localDtm from to)) rawRawData


initZed :: IO Zeiterfassungsdaten
initZed = do
  now <- Zeit.getCurrentLocalTime
  preRd <- parseFile constPath
  let rd = diffMachine now preRd
  return Zeiterfassungsdaten
    { rawData = rd
    , hoursPerIntervall = []
    , hasActiveLog = False
    }