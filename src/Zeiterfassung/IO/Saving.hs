{-# LANGUAGE OverloadedStrings #-}

module Zeiterfassung.IO.Saving
  ( writeZed ) where

import qualified Data.List as List
import qualified Data.Maybe as Maybe
import Data.Time.LocalTime
import Zeiterfassung.Data
import qualified Zeiterfassung.IO.Common as Common


spanToString :: Maybe LocalTime -> Maybe LocalTime -> String
spanToString von bis =
  let
    maybeDtmToStr = Maybe.maybe "nothing" dateTimeToString
    vonStr = maybeDtmToStr von
    bisStr = maybeDtmToStr bis
  in vonStr ++ " - " ++ bisStr


rawDataToString :: [(Maybe LocalTime, Maybe LocalTime)] -> String
rawDataToString rd = List.intercalate "\n" . reverse $ fmap (uncurry spanToString) rd


writeZed :: Zeiterfassungsdaten -> IO ()
writeZed zed = do
  let rawDataStr = rawDataToString . rawData $ zed
  writeFile (Common.constPath ++ "-") rawDataStr
