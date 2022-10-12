{-# LANGUAGE OverloadedStrings #-}

module UI.Draw (drawUI, myAttrMap) where

import Brick
  ( App(..), AttrMap, BrickEvent(..), EventM, Next, Widget
  , customMain, neverShowCursor
  , continue, halt
  , str
  , attrMap, withAttr, emptyWidget, AttrName, on
  , (<+>), (<=>)
  , padTopBottom, padLeftRight
  )
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as Center
import qualified Brick.Widgets.Core as Core
import qualified Brick.Widgets.Edit as Edit
import qualified Brick.Widgets.List as L
import Data.DateTime
import Data.Text
import qualified Data.Sequence as Seq

import UI.State
import qualified Zeiterfassung.Aggregations as Aggregations
import Zeiterfassung.Data
import qualified Graphics.Vty as V



drawUI :: ZeiterfassungsdatenTUI -> [Widget Name]
drawUI z =
  [
    (
      (Core.hLimit 51 . B.borderWithLabel (str "Rohdatensätze") . drawTimes $ z)
      <+> Core.hLimit 47 ((B.borderWithLabel (str "Aggregierte Zahlen") . drawAggregatedDetails $ z)
          <=> (B.borderWithLabel (str "Wochenzeiten") . drawWochenzeiten $ z))
      <+> (B.borderWithLabel (str "Datensatz") . drawDatensatz $ z)
    )
    <=> padLeftRight 2 drawHelp
    <=> (padLeftRight 2 . drawSystem $ z)
  ]


drawTimes :: ZeiterfassungsdatenTUI -> Widget Name
drawTimes z =
  let
    genericList = rawDataGenericList z
    widgetList :: Widget Name
    widgetList = L.renderList drawSingleTimePair True genericList
  in widgetList


drawSingleTimePair :: Bool -> (Maybe DateTime, Maybe DateTime, DateTimeDiff) -> Widget Name
drawSingleTimePair isSelected (von, bis, diff) =
  let
    prepareString Nothing = "       ??       "
    prepareString (Just dtm) = Prelude.take 16 . show $ dtm
    diffString = show diff
    diffStringPadded = Prelude.replicate (6 - Prelude.length diffString) ' ' ++ diffString
    timeSpanString = prepareString von ++ " - " ++ prepareString bis ++ "  │ " ++ diffStringPadded
    modificator = if isSelected then withAttr selectedDatePairAttr else id
  in Center.hCenter . modificator . str $ timeSpanString


drawAggregatedDetails :: ZeiterfassungsdatenTUI -> Widget Name
drawAggregatedDetails z =
  let
    pad = padTopBottom 1 . padLeftRight 4
    workedHours = str . Prelude.take 6 . show . Aggregations.sumAllHours . rawDataWithDiff . zed $ z
    workedHoursPerWeek = str . Prelude.take 4 . show . Aggregations.averageHoursPerWeek . rawDataWithDiff . zed $ z
    dataFields = ((str . show . Prelude.length . rawData . zed $ z) <=> workedHours <=> workedHoursPerWeek)
    texts = str "Anzahl an Datensätzen:" <=> str "Gesamtzahl der Stunden:" <=> str "Stunden pro Woche:"
  in pad texts <+> pad dataFields


drawWochenzeiten :: ZeiterfassungsdatenTUI -> Widget Name
drawWochenzeiten z =
  let
    hoursPerWeek = Seq.fromList . Aggregations.weeklyHours . rawDataWithDiff . zed $ z
    genericList = L.list 2 hoursPerWeek 1
    widgetList :: Widget Name
    widgetList = L.renderList drawSingleWeek True genericList
  in widgetList


drawSingleWeek :: Bool -> (Int, DateTimeDiff) -> Widget Name
drawSingleWeek _ (weekNumber, diff) =
  let
    diffString = show diff
    diffStringPadded = Prelude.replicate (6 - Prelude.length diffString) ' ' ++ diffString
    timeSpanString = show weekNumber ++ "  │ " ++ diffStringPadded
  in Center.hCenter . str $ timeSpanString


drawSingleDateTime :: Edit.Editor Text Name -> Bool -> Widget Name
drawSingleDateTime editor isFocus =
  let
    drawContent [] = str "?"
    drawContent (h:_) = str . show $ h
  in padTopBottom 1 . padLeftRight 4 . Edit.renderEditor drawContent isFocus $ editor


drawDatensatz :: ZeiterfassungsdatenTUI -> Widget Name
drawDatensatz z =
  let
    editorWidgetVon = drawSingleDateTime (editorVon z) (focus z == FocusVon)
    textVon = if focus z == FocusVon then " [Von:]" else "  Von: "
    editorWidgetBis = drawSingleDateTime (editorBis z) (focus z == FocusBis)
    textBis = if focus z == FocusBis then " [Bis:]" else "  Bis: "
 in (str textVon <+> editorWidgetVon) <=> (str textBis <+> editorWidgetBis)


drawHelp :: Widget Name
drawHelp = str "V=Fokus auf von; B=Fokus auf bis; Esc=Fokus auf Liste; R=Neuladen; Q=Beenden"


drawSystem :: ZeiterfassungsdatenTUI -> Widget Name
drawSystem z =
  let
    lastFetchStr = Prelude.take 19 . show . lastFetch $ z
    vonFocusStr = "vonFocus: " ++ (if focus z == FocusVon then "1" else "0")
    bisFocusStr = "bisFocus: " ++ (if focus z == FocusBis then "1" else "0")
  in str ("Letzte Aktualisierung: " ++ lastFetchStr ++ " | " ++ vonFocusStr ++ " | " ++ bisFocusStr)


myAttrMap :: AttrMap
myAttrMap = attrMap V.defAttr
  [ (selectedDatePairAttr, V.white `on` V.blue)
  ]

selectedDatePairAttr :: AttrName
selectedDatePairAttr = "selectedDatePairAttr"