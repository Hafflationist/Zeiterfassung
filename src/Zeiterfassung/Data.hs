module Zeiterfassung.Data
  ( Zeiterfassungsdaten(..)
  , RawData
  , RawDataWithDiff
  , IntervallData
  , DateTimeDiff(..)
  ) where

import Data.DateTime
import Data.Time
import Data.Time.Clock.POSIX

data Zeiterfassungsdaten = Zeiterfassungsdaten
  { rawData           :: RawData
  , rawDataWithDiff   :: RawDataWithDiff
  , hoursPerIntervall :: IntervallData
  , hasActiveLog      :: Bool
  } deriving (Show, Eq)

type RawData = [(Maybe DateTime, Maybe DateTime)]

type RawDataWithDiff = [(Maybe DateTime, Maybe DateTime, DateTimeDiff)]

type IntervallData = [(String, Int)]

newtype DateTimeDiff = DTD NominalDiffTime
  deriving (Eq)
instance Show DateTimeDiff where

  show (DTD ndt) = formatTime defaultTimeLocale "%H:%M" . posixSecondsToUTCTime $ ndt

instance Num DateTimeDiff where
  (+) (DTD ndt1) (DTD ndt2) = DTD (ndt1 + ndt2)
  (*) (DTD ndt1) (DTD ndt2) = DTD (ndt1 * ndt2)
  abs (DTD ndt) = DTD (abs ndt)
  signum (DTD ndt) = DTD (signum ndt)
  fromInteger n = DTD (fromInteger n)
  (-) (DTD ndt1) (DTD ndt2) = DTD (ndt1 - ndt2)