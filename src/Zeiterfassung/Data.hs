module Zeiterfassung.Data
  ( Zeiterfassungsdaten(..)
  , RawData
  , IntervallData
  ) where

import Data.DateTime
import Data.Time.Clock

data Zeiterfassungsdaten = Zeiterfassungsdaten
  { rawData           :: RawData
  , workedHours       :: Double
  , hoursPerIntervall :: IntervallData
  , hasActiveLog      :: Bool
  } deriving (Show, Eq)

type RawData = [(DateTime, DateTime, NominalDiffTime)]

type IntervallData = [(String, Int)]