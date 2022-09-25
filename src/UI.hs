{-# LANGUAGE OverloadedStrings #-}

module UI (main) where

import Brick
  ( App(..), AttrMap, BrickEvent(..), EventM, Next, Widget
  , customMain, neverShowCursor
  , continue, halt
  , str
  , attrMap, withAttr, emptyWidget, AttrName, on
  , (<+>), (<=>)
  , padTopBottom, padLeftRight
  )
import Brick.BChan (newBChan, writeBChan)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as Center
import qualified Brick.Widgets.Core as Core
import qualified Brick.Widgets.List as L
import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay, forkIO)
import Data.DateTime
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Time as Time
import Data.Time.LocalTime

import qualified Zeiterfassung.Aggregations as Aggregations
import Zeiterfassung.Data
import qualified Zeiterfassung.Parsing as Parsing
import qualified Zeiterfassung.Zeit as Zeit
import qualified Graphics.Vty as V

data Tick = Tick

type Name = Int

data ZeiterfassungsdatenTUI = ZeiterfassungsdatenTUI
  { zed                :: Zeiterfassungsdaten
  , rawDataGenericList :: L.GenericList Name Seq (DateTime, DateTime, DateTimeDiff)
  , lastFetch          :: LocalTime
  } deriving (Show)

-- App definition

app :: App ZeiterfassungsdatenTUI Tick Name
app = App { appDraw = drawUI
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return
          , appAttrMap = const myAttrMap
          }

main :: IO ()
main = do
  let secondsPerTick = 2
  chan <- newBChan 10
  _ <- forkIO $ forever $ do
    writeBChan chan Tick
    threadDelay (1000000 * secondsPerTick)
  g <- initZedTui
  let builder = V.mkVty V.defaultConfig
  initialVty <- builder
  void $ customMain initialVty builder (Just chan) app g




initZedTui :: IO ZeiterfassungsdatenTUI
initZedTui = do
  z <- Parsing.initZed
  now <- Zeit.getCurrentLocalTime
  return . tuifyZed now Nothing $ z


reloadZedTui :: ZeiterfassungsdatenTUI -> IO ZeiterfassungsdatenTUI
reloadZedTui oldZ = do
  let selectedElement = fmap snd . L.listSelectedElement . rawDataGenericList $ oldZ
  z <- Parsing.initZed
  now <- Zeit.getCurrentLocalTime
  return . tuifyZed now selectedElement $ z


tuifyZed :: LocalTime -> Maybe (DateTime, DateTime, DateTimeDiff) -> Zeiterfassungsdaten -> ZeiterfassungsdatenTUI
tuifyZed now Nothing z =
  let
    currentRawData = Seq.fromList . rawData $ z
    genericList = L.list 1 currentRawData 1
  in ZeiterfassungsdatenTUI {
    zed = z,
    rawDataGenericList = genericList,
    lastFetch = now
  }
tuifyZed now (Just targetElement) z =
  let
    currentRawData = Seq.fromList . rawData $ z
    genericList = L.listMoveToElement targetElement . L.list 1 currentRawData $ 1
  in ZeiterfassungsdatenTUI {
    zed = z,
    rawDataGenericList = genericList,
    lastFetch = now
  }


-- Handling events

handleEvent :: ZeiterfassungsdatenTUI -> BrickEvent Name Tick -> EventM Name (Next ZeiterfassungsdatenTUI)
handleEvent z (AppEvent Tick)                       = liftIO (reloadZedTui z) >>= continue
handleEvent z (VtyEvent (V.EvKey (V.KChar 'r') [])) = liftIO (reloadZedTui z) >>= continue
handleEvent z (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt z
handleEvent z (VtyEvent (V.EvKey V.KEsc []))        = halt z
handleEvent z (VtyEvent e) = do
  newRawDataGenericList <- L.handleListEvent e (rawDataGenericList z)
  continue (z {
    rawDataGenericList = newRawDataGenericList
  })
handleEvent z _                                     = continue z


-- Drawing

drawUI :: ZeiterfassungsdatenTUI -> [Widget Name]
drawUI z =
  [
    (Core.hLimit 51 . B.borderWithLabel (str "Rohdatensätze") . drawTimes $ z)
    <+> Core.hLimit 47 ((B.borderWithLabel (str "Aggregierte Zahlen") . drawAggregatedDetails $ z) 
        <=> (B.borderWithLabel (str "Wochenzeiten") . drawWochenzeiten $ z))
    <+> (padTopBottom 1 . padLeftRight 4 . drawSystem $ z)
  ]


drawTimes :: ZeiterfassungsdatenTUI -> Widget Name
drawTimes z =
  let
    genericList = rawDataGenericList z
    widgetList :: Widget Name
    widgetList = L.renderList drawSingleTimePair True genericList
  in widgetList


drawSingleTimePair :: Bool -> (DateTime, DateTime, DateTimeDiff) -> Widget Name
drawSingleTimePair isSelected (von, bis, diff) =
  let
    prepareString = take 16 . show
    diffString = show diff
    diffStringPadded = replicate (6 - length diffString) ' ' ++ diffString
    timeSpanString = prepareString von ++ " - " ++ prepareString bis ++ "  │ " ++ diffStringPadded
    modificator = if isSelected then withAttr selectedDatePairAttr else id
  in Center.hCenter . modificator . str $ timeSpanString


drawAggregatedDetails :: ZeiterfassungsdatenTUI -> Widget Name
drawAggregatedDetails z =
  let
    pad = padTopBottom 1 . padLeftRight 4
    workedHours = str . take 6 . show . Aggregations.sumAllHours . zed $ z
    workedHoursPerWeek = str . take 4 . show . Aggregations.averageHoursPerWeek . zed $ z
    dataFields = ((str . show . length . rawData . zed $ z) <=> workedHours <=> workedHoursPerWeek)
    texts = str "Anzahl an Datensätzen:" <=> str "Gesamtzahl der Stunden:" <=> str "Stunden pro Woche:"
  in pad texts <+> pad dataFields


drawWochenzeiten :: ZeiterfassungsdatenTUI -> Widget Name
drawWochenzeiten z =
  let
    hoursPerWeek = Seq.fromList . Aggregations.weeklyHours . zed $ z
    genericList = L.list 2 hoursPerWeek 1
    widgetList :: Widget Name
    widgetList = L.renderList drawSingleWeek True genericList
  in widgetList


drawSingleWeek :: Bool -> (Int, DateTimeDiff) -> Widget Name
drawSingleWeek _ (weekNumber, diff) =
  let
    diffString = show diff
    diffStringPadded = replicate (6 - length diffString) ' ' ++ diffString
    timeSpanString = show weekNumber ++ "  │ " ++ diffStringPadded
  in Center.hCenter . str $ timeSpanString


drawSystem :: ZeiterfassungsdatenTUI -> Widget Name
drawSystem z =
  let
    lastFetchStr = take 19 . show . lastFetch $ z
  in str ("Letzte Aktualisierung: " ++ lastFetchStr)


myAttrMap :: AttrMap
myAttrMap = attrMap V.defAttr
  [ (selectedDatePairAttr, V.white `on` V.blue)
  ]

selectedDatePairAttr :: AttrName
selectedDatePairAttr = "selectedDatePairAttr"