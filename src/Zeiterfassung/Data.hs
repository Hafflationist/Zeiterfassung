module Zeiterfassung.Data
  ( Zeiterfassungsdaten(..)
  , RawData
  , RawDataWithDiff
  , IntervallData
  , DateTimeDiff(..)
  , dateTimeToString
  , stringToLocalTime
  ) where

import Data.Attoparsec.Text
import Data.Attoparsec.Time
-- import Data.DateTime ( DateTime )
import qualified Data.Either.Extra as EitherExtra
import qualified Data.Text as Txt
import Data.Time
import Data.Time.Clock.POSIX

data Zeiterfassungsdaten = Zeiterfassungsdaten
  { rawData           :: RawData
  , rawDataWithDiff   :: RawDataWithDiff
  , hoursPerIntervall :: IntervallData
  , hasActiveLog      :: Bool
  } deriving (Show, Eq)

type RawData = [(Maybe LocalTime, Maybe LocalTime)]

type RawDataWithDiff = [(Maybe LocalTime, Maybe LocalTime, DateTimeDiff)]

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


dateTimeToString :: LocalTime -> String 
dateTimeToString dtm =
  let
    repl ' ' = 'T'
    repl x = x
  in fmap repl
   . Prelude.take 16
   . show
   $ dtm

stringToLocalTime :: String -> Maybe LocalTime
stringToLocalTime str =
  let
    repl ' ' = 'T'
    repl x = x
  in EitherExtra.eitherToMaybe
    . parseOnly localTime
    . Txt.pack
    . fmap repl
    $ (str ++ ":00Z")
