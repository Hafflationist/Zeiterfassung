{-# LANGUAGE OverloadedStrings #-}

module Zeiterfassung.IO.Saving
  ( writeZed ) where

import qualified Data.List as List
import Data.DateTime
import Zeiterfassung.Data


constPath :: FilePath
constPath = "/Schnitt/Uni/MA/talog.txt"


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
rawDataToString rd = List.intercalate "\n" (fmap (uncurry spanToString) rd)


writeZed :: Zeiterfassungsdaten -> IO ()
writeZed zed = do
  let rawDataStr = rawDataToString . fmap (\ (von, bis, _) -> (von, bis)) . rawData $ zed
  writeFile constPath rawDataStr