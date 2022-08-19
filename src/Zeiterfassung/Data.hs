module Zeiterfassung.Data
  ( Zeiterfassungsdaten(..)
  , RawData
  , IntervallData
  , DateTimeDiff(..)
  ) where

import Data.DateTime
import Data.Time
import Data.Time.Clock.POSIX

data Zeiterfassungsdaten = Zeiterfassungsdaten
  { rawData           :: RawData
  , workedHours       :: Double
  , hoursPerIntervall :: IntervallData
  , hasActiveLog      :: Bool
  } deriving (Show, Eq)

type RawData = [(DateTime, DateTime, DateTimeDiff)]

type IntervallData = [(String, Int)]

newtype DateTimeDiff = DTD NominalDiffTime
  deriving (Eq)

instance Show DateTimeDiff where
  show (DTD ndt) = formatTime defaultTimeLocale "%H:%M" . posixSecondsToUTCTime $ ndt