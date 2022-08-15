module Zeiterfassung.Data
  ( Zeiterfassungsdaten(..)
  , RawData
  , IntervallData
  ) where

import Data.DateTime


data Zeiterfassungsdaten = Zeiterfassungsdaten
  { rawData           :: RawData
  , workedHours       :: Double
  , hoursPerIntervall :: IntervallData
  , hasActiveLog      :: Bool
  } deriving (Show, Eq)

type RawData = [(DateTime, DateTime)]

type IntervallData = [(String, Int)]