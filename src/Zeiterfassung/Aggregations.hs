module Zeiterfassung.Aggregations
  (sumAllHours) where

import Data.List as List ( map, sum )
import Prelude
import Zeiterfassung.Data (Zeiterfassungsdaten (..), DateTimeDiff (..))


sumAllHours :: Zeiterfassungsdaten -> Float
sumAllHours z = (List.sum . List.map (\(_,_, DTD d) -> fromInteger . round $ d) . rawData $ z) / 3600.0