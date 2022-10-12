{-# LANGUAGE OverloadedStrings #-}

module Zeiterfassung.IO.Saving
  ( writeZed ) where

import qualified Data.List as List
import Data.DateTime
import Zeiterfassung.Data
import qualified Zeiterfassung.IO.Common as Common


dateTimeToString :: Maybe DateTime -> String 
dateTimeToString Nothing = "nothing"
dateTimeToString (Just dtm) =
  let
    repl ' ' = 'T'
    repl x = x  
  in fmap repl 
   . Prelude.take 16 
   . show 
   $ dtm


spanToString :: Maybe DateTime -> Maybe DateTime -> String
spanToString von bis =
  let
    vonStr = dateTimeToString von
    bisStr = dateTimeToString bis
  in vonStr ++ " - " ++ bisStr


rawDataToString :: [(Maybe DateTime, Maybe DateTime)] -> String
rawDataToString rd = List.intercalate "\n" . reverse $ fmap (uncurry spanToString) rd


writeZed :: Zeiterfassungsdaten -> IO ()
writeZed zed = do
  let rawDataStr = rawDataToString . rawData $ zed
  writeFile (Common.constPath ++ "-") rawDataStr