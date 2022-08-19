module Zeiterfassung.Data
  ( Zeiterfassungsdaten(..)
  , RawData
  , IntervallData
  ) where

import Data.DateTime
import Data.Time.Clock

data Zeiterfassungsdaten = Zeiterfassungsdaten
  { rawData           :: RawData
  , rawDiffs          :: [NominalDiffTime]
  , workedHours       :: Double
  , hoursPerIntervall :: IntervallData
  , hasActiveLog      :: Bool
  } deriving (Show, Eq)

type RawData = [(DateTime, DateTime)]

type IntervallData = [(String, Int)]