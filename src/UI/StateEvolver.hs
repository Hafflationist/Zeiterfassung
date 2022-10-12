{-# LANGUAGE OverloadedStrings #-}

module UI.StateEvolver (main) where

import Brick
  ( App(..), AttrMap, BrickEvent(..), EventM, Next, Widget
  , customMain, neverShowCursor
  , continue, halt
  , str
  , attrMap, withAttr, emptyWidget, AttrName, on
  , (<+>), (<=>)
  , padTopBottom, padLeftRight, Viewport (_vpLeft)
  )
import Brick.BChan (newBChan, writeBChan)
import qualified Brick.Widgets.Edit as Edit
import qualified Brick.Widgets.List as L
import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay, forkIO)
import Data.DateTime
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Text
import qualified Data.Text.Zipper as Zipper
import Data.Time.LocalTime

import qualified UI.Draw as Draw
import UI.State
import Zeiterfassung.Data
import qualified Zeiterfassung.IO.Saving as Saving
import qualified Zeiterfassung.IO.Parsing as Parsing
import qualified Zeiterfassung.Zeit as Zeit
import qualified Graphics.Vty as V
import qualified Zeiterfassung.Aggregations as Aggregations

-- App definition

data Tick = Tick


app :: App ZeiterfassungsdatenTUI Tick Name
app = App { appDraw = Draw.drawUI
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return
          , appAttrMap = const Draw.myAttrMap
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
  return . tuifyZed now z $ FocusListe


refreshZedTui :: ZeiterfassungsdatenTUI -> IO ZeiterfassungsdatenTUI
refreshZedTui z = do
  now <- Zeit.getCurrentLocalTime
  let newZed = Aggregations.diffMachine now . zed $ z
  let selectedElementIdx = Maybe.fromMaybe 0 . L.listSelected . rawDataGenericList $ z
  let currentRawData = Seq.fromList . rawDataWithDiff $ newZed 
  let genericList = L.listMoveTo selectedElementIdx . L.list 1 currentRawData $ 1
  return (z {
    zed = newZed,
    rawDataGenericList = genericList
  })


reloadZedTui :: ZeiterfassungsdatenTUI -> IO ZeiterfassungsdatenTUI
reloadZedTui oldZ = do
  now <- Zeit.getCurrentLocalTime
  let firstElement = List.head . rawDataWithDiff . zed $ oldZ
  let selectedElement = Maybe.maybe firstElement snd . L.listSelectedElement . rawDataGenericList $ oldZ
  z <- Parsing.initZed
  let currentRawData = Seq.fromList . rawDataWithDiff $ z 
  let genericList = L.listMoveToElement selectedElement . L.list 1 currentRawData $ 1
  return . reloadEditors $ (oldZ {
    zed = z,
    rawDataGenericList = genericList,
    lastFetch = now
  })


saveZedTui :: ZeiterfassungsdatenTUI -> IO ZeiterfassungsdatenTUI
saveZedTui z = do
  Saving.writeZed . zed $ z
  return z


reloadEditors :: ZeiterfassungsdatenTUI -> ZeiterfassungsdatenTUI
reloadEditors z =
  let (newEditorVon, newEditorBis) = getRightEditors . L.listSelectedElement . rawDataGenericList $ z 
  in z {
    editorVon = newEditorVon,
    editorBis = newEditorBis
  }
  where
    getRightEditors (Just (_, (Just von, Just bis, _))) = getRightEditorsInner (show von) (show bis)
    getRightEditors (Just (_, (Nothing, Just bis, _))) = getRightEditorsInner "??" (show bis)
    getRightEditors (Just (_, (Just von, Nothing, _))) = getRightEditorsInner (show von) "??"
    getRightEditors _ = getRightEditorsInner "??" "??"
    getRightEditorsInner von bis = (
        moveCursorToEnd . Edit.editorText 3 (Just 1) $ pack . Prelude.take 16 $ von, 
        moveCursorToEnd . Edit.editorText 4 (Just 1) $ pack . Prelude.take 16 $ bis
      )
    

moveCursorToEnd :: Edit.Editor Text Name -> Edit.Editor Text Name
moveCursorToEnd = Edit.applyEdit Zipper.gotoEOF


setFocus :: Focus -> ZeiterfassungsdatenTUI -> ZeiterfassungsdatenTUI
setFocus newFocus zTui =
  zTui {
    focus = newFocus
  }


tuifyZed :: LocalTime -> Zeiterfassungsdaten -> Focus -> ZeiterfassungsdatenTUI
tuifyZed now z foc =
  let
    currentRawData = Seq.fromList . rawDataWithDiff $ z
    genericList = L.list 1 currentRawData 1
  in ZeiterfassungsdatenTUI {
    zed = z,
    rawDataGenericList = genericList,
    editorVon = moveCursorToEnd . Edit.editorText 3 (Just 1) $ "Von",
    editorBis = moveCursorToEnd . Edit.editorText 4 (Just 1) $ "Bis",
    lastFetch = now,
    focus = foc
  }


handleListThings :: ZeiterfassungsdatenTUI -> V.Event -> EventM Name (Next ZeiterfassungsdatenTUI)
handleListThings z e = do
  newRawDataGenericList <- L.handleListEvent e (rawDataGenericList z)
  continue . reloadEditors $ (z {
    rawDataGenericList = newRawDataGenericList
  })


handleEditorVon :: V.Event -> ZeiterfassungsdatenTUI -> EventM Name (Next ZeiterfassungsdatenTUI)
handleEditorVon event z = do
  newEditorVon <- Edit.handleEditorEvent event (editorVon z)
  continue (z {
    editorVon = newEditorVon
  })


handleEditorBis :: V.Event -> ZeiterfassungsdatenTUI -> EventM Name (Next ZeiterfassungsdatenTUI)
handleEditorBis event z = do
  newEditorBis <- Edit.handleEditorEvent event (editorBis z)
  continue (z {
    editorBis = newEditorBis
  })


-- Handling events

handleEvent :: ZeiterfassungsdatenTUI -> BrickEvent Name Tick -> EventM Name (Next ZeiterfassungsdatenTUI)
handleEvent z (AppEvent Tick)                        = liftIO (refreshZedTui z) >>= continue
handleEvent z (VtyEvent (V.EvKey (V.KChar 'r') []))  = liftIO (reloadZedTui z) >>= continue
handleEvent z (VtyEvent (V.EvKey (V.KChar 's') []))  = liftIO (saveZedTui z) >>= continue
handleEvent z (VtyEvent (V.EvKey (V.KChar 'v') []))  = continue . setFocus FocusVon $ z
handleEvent z (VtyEvent (V.EvKey (V.KChar 'b') []))  = continue . setFocus FocusBis $ z
handleEvent z (VtyEvent (V.EvKey V.KEsc []))         = continue . setFocus FocusListe $ z
handleEvent z (VtyEvent (V.EvKey (V.KChar 'q') []))  = halt z
handleEvent z (VtyEvent e)
  | focus z == FocusListe                            = handleListThings z e
handleEvent z (VtyEvent e)
  | focus z == FocusVon                              = handleEditorVon e z
  | focus z == FocusBis                              = handleEditorBis e z
--  | otherwise                                       = 
handleEvent z _                                      = continue z