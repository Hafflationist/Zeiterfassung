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

import qualified Zeiterfassung.Aggregations as Aggregations
import Zeiterfassung.Data
import qualified Zeiterfassung.Parsing as Parsing
import qualified Graphics.Vty as V

data Tick = Tick

type Name = ()

data ZeiterfassungsdatenTUI = ZeiterfassungsdatenTUI
  { zed                :: Zeiterfassungsdaten
  , rawDataGenericList :: L.GenericList Name Seq (DateTime, DateTime, DateTimeDiff)
  , lastFetch          :: DateTime
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
  chan <- newBChan 10
  _ <- forkIO $ forever $ do
    writeBChan chan Tick
    threadDelay 100000 -- decides how fast your Zeiterfassungsdaten moves
  g <- initZedTui
  let builder = V.mkVty V.defaultConfig
  initialVty <- builder
  void $ customMain initialVty builder (Just chan) app g


initZedTui :: IO ZeiterfassungsdatenTUI
initZedTui = do
  z <- Parsing.initZed
  now <- Time.getCurrentTime
  return . tuifyZed now $ z


tuifyZed :: DateTime -> Zeiterfassungsdaten -> ZeiterfassungsdatenTUI
tuifyZed now z =
  let
    currentRawData = Seq.fromList . rawData $ z
    genericList = L.list () currentRawData 1
  in ZeiterfassungsdatenTUI {
    zed = z,
    rawDataGenericList = genericList,
    lastFetch = now
  }


-- Handling events

handleEvent :: ZeiterfassungsdatenTUI -> BrickEvent Name Tick -> EventM Name (Next ZeiterfassungsdatenTUI)
handleEvent z (AppEvent Tick)                       = continue z
handleEvent z (VtyEvent (V.EvKey (V.KChar 'r') [])) = liftIO initZedTui >>= continue
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
    <+> (B.borderWithLabel (str "Aggregierte Zahlen") . drawAggregatedDetails $ z)
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
    workedHours = str . show . Aggregations.sumAllHours . zed $ z
    dataFields = ((str . show . length . rawData . zed $ z) <=> workedHours)
    texts = str "Anzahl an Datensätzen:" <=> str "Gesamtzahl der Stunden:" <=> str "Stunden pro Woche:"
  in pad texts <+> pad dataFields


drawSystem :: ZeiterfassungsdatenTUI -> Widget Name
drawSystem z =
  let
    lastFetchStr = show . lastFetch $ z
  in str ("Letzte Aktualisierung: " ++ lastFetchStr)


myAttrMap :: AttrMap
myAttrMap = attrMap V.defAttr
  [ (selectedDatePairAttr, V.white `on` V.blue)
  ]

selectedDatePairAttr :: AttrName
selectedDatePairAttr = "selectedDatePairAttr"