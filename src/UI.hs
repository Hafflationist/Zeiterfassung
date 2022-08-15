{-# LANGUAGE OverloadedStrings #-}

module UI (main) where

import Brick
  ( App(..), AttrMap, BrickEvent(..), EventM, Next, Widget
  , customMain, neverShowCursor
  , continue, halt
  , str
  , attrMap, withAttr, emptyWidget, AttrName, on
  , (<+>)
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

import Zeiterfassung
import qualified Graphics.Vty as V
-- Types

data Tick = Tick

type Name = ()

data ZeiterfassungsdatenTUI = ZeiterfassungsdatenTUI
  { zed                :: Zeiterfassungsdaten
  , rawDataGenericList :: L.GenericList Name Seq (DateTime, DateTime)
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
  z <- initGame
  return . tuifyZed $ z


tuifyZed :: Zeiterfassungsdaten -> ZeiterfassungsdatenTUI
tuifyZed z = 
  let 
    currentRawData = Seq.fromList . rawData $ z
    genericList = L.list () currentRawData 1
  in ZeiterfassungsdatenTUI {
    zed = z,
    rawDataGenericList = genericList
  }


-- Handling events

handleEvent :: ZeiterfassungsdatenTUI -> BrickEvent Name Tick -> EventM Name (Next ZeiterfassungsdatenTUI)
handleEvent z (AppEvent Tick)                       = continue z
-- handleEvent g (VtyEvent (V.EvKey V.KUp []))         = continue $ turn North g
-- handleEvent g (VtyEvent (V.EvKey V.KDown []))       = continue $ turn South g
-- handleEvent g (VtyEvent (V.EvKey V.KRight []))      = continue $ turn East g
-- handleEvent g (VtyEvent (V.EvKey V.KLeft []))       = continue $ turn West g
-- handleEvent g (VtyEvent (V.EvKey (V.KChar 'k') [])) = continue $ turn North g
-- handleEvent g (VtyEvent (V.EvKey (V.KChar 'j') [])) = continue $ turn South g
-- handleEvent g (VtyEvent (V.EvKey (V.KChar 'l') [])) = continue $ turn East g
-- handleEvent g (VtyEvent (V.EvKey (V.KChar 'h') [])) = continue $ turn West g
handleEvent z (VtyEvent (V.EvKey (V.KChar 'r') [])) = liftIO initZedTui >>= continue
handleEvent z (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt z
handleEvent z (VtyEvent (V.EvKey V.KEsc []))        = halt z
handleEvent z (VtyEvent e) = do
  hugo <- L.handleListEvent e (rawDataGenericList z)
  continue (z {
    rawDataGenericList = hugo
  })
handleEvent z _                                     = continue z


-- Drawing

drawUI :: ZeiterfassungsdatenTUI -> [Widget Name]
drawUI g =
  [ (Core.hLimit 41 . B.border . drawTimes $ g)  -- 37 Breite ist das absolute Minimum
    <+> (B.border . drawGrid $ g) 
    <+> B.border emptyWidget
  ]


drawTimes :: ZeiterfassungsdatenTUI -> Widget Name
drawTimes z =
  let 
    genericList = rawDataGenericList z
    widgetList :: Widget Name
    widgetList = L.renderList drawSingleTimePair True genericList
  in widgetList


drawSingleTimePair :: Bool -> (DateTime, DateTime) -> Widget Name
drawSingleTimePair isSelected (von, bis) = 
  let
    prepareString = take 16 . show
    timeSpanString = prepareString von ++ " - " ++ prepareString bis
    modificator = if isSelected then withAttr selectedDatePairAttr else id
  in Center.hCenter . modificator . str $ timeSpanString


drawGrid :: ZeiterfassungsdatenTUI -> Widget Name
drawGrid = str . show . length . rawData . zed


myAttrMap :: AttrMap
myAttrMap = attrMap V.defAttr
  [ (selectedDatePairAttr, V.white `on` V.blue)
  ]

selectedDatePairAttr :: AttrName
selectedDatePairAttr = "selectedDatePairAttr"