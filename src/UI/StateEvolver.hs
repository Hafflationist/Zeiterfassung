{-# LANGUAGE OverloadedStrings #-}

module UI.StateEvolver (main) where

import Brick
  ( App(..), BrickEvent(..), EventM
  , customMain, neverShowCursor
  , halt
  , get, put
  )
import Brick.BChan (newBChan, writeBChan)
import qualified Brick.Widgets.Edit as Edit
import qualified Brick.Widgets.List as L
import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay, forkIO)
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import Data.Text ( Text, pack)
import qualified Data.Text.Zipper as Zipper
import Data.Time.LocalTime ( LocalTime )

import Lens.Micro.Mtl (zoom)

import qualified UI.Draw as Draw
import UI.State
import Zeiterfassung.Data ( Zeiterfassungsdaten(rawDataWithDiff) )
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
          , appStartEvent = return ()
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


refreshZedTui :: LocalTime -> EventM Name ZeiterfassungsdatenTUI ()
refreshZedTui now = do
  z <- get
  let newZed = Aggregations.diffMachine now . _zed $ z
  let selectedElementIdx = Maybe.fromMaybe 0 . L.listSelected . _rawDataGenericList $ z
  let currentRawData = Seq.fromList . rawDataWithDiff $ newZed 
  let genericList = L.listMoveTo selectedElementIdx . L.list 1 currentRawData $ 1
  put (z {
    _zed = newZed,
    _rawDataGenericList = genericList
  })


reloadPrimitives :: IO (LocalTime, Zeiterfassungsdaten)
reloadPrimitives = do
  now <- Zeit.getCurrentLocalTime
  z <- Parsing.initZed
  return (now, z)

reloadZedTui :: (LocalTime, Zeiterfassungsdaten) -> EventM Name ZeiterfassungsdatenTUI ()
reloadZedTui (now, z) = do
  oldZ <- get
  let firstElement = List.head . rawDataWithDiff . _zed $ oldZ
  let selectedElement = Maybe.maybe firstElement snd . L.listSelectedElement . _rawDataGenericList $ oldZ
  let currentRawData = Seq.fromList . rawDataWithDiff $ z 
  let genericList = L.listMoveToElement selectedElement . L.list 1 currentRawData $ 1
  put . reloadEditors $ (oldZ {
    _zed = z,
    _rawDataGenericList = genericList,
    _lastFetch = now
  })


saveZedTui :: EventM Name ZeiterfassungsdatenTUI (IO ())
saveZedTui = do
  z <- get
  return . Saving.writeZed . _zed $ z


reloadEditors :: ZeiterfassungsdatenTUI -> ZeiterfassungsdatenTUI
reloadEditors z =
  let (newEditorVon, newEditorBis) = getRightEditors . L.listSelectedElement . _rawDataGenericList $ z 
  in z {
    _editorVon = newEditorVon,
    _editorBis = newEditorBis
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


setFocus :: Focus -> EventM Name ZeiterfassungsdatenTUI ()
setFocus newFocus = do
  zTui <- get
  put (zTui {
    _focus = newFocus
  })


tuifyZed :: LocalTime -> Zeiterfassungsdaten -> Focus -> ZeiterfassungsdatenTUI
tuifyZed now z foc =
  let
    currentRawData = Seq.fromList . rawDataWithDiff $ z
    genericList = L.list 1 currentRawData 1
  in ZeiterfassungsdatenTUI {
    _zed = z,
    _rawDataGenericList = genericList,
    _editorVon = moveCursorToEnd . Edit.editorText 3 (Just 1) $ "Von",
    _editorBis = moveCursorToEnd . Edit.editorText 4 (Just 1) $ "Bis",
    _lastFetch = now,
    _focus = foc
  }


handle :: V.Event -> EventM Name ZeiterfassungsdatenTUI ()
handle e = do
  z <- get
  let focusOfZ = _focus z
  handleWithFocus focusOfZ e

handleWithFocus :: Focus -> V.Event -> EventM Name ZeiterfassungsdatenTUI ()
handleWithFocus FocusListe e = zoom rawDataGenericList $ L.handleListEvent e
handleWithFocus FocusVon e = zoom editorVon . Edit.handleEditorEvent . VtyEvent $ e
handleWithFocus FocusBis e = zoom editorBis . Edit.handleEditorEvent . VtyEvent $ e


-- Handling events

handleEvent :: BrickEvent Name Tick -> EventM Name ZeiterfassungsdatenTUI ()
handleEvent (AppEvent Tick)                        = liftIO Zeit.getCurrentLocalTime >>= refreshZedTui
handleEvent (VtyEvent (V.EvKey (V.KChar 'r') []))  = liftIO reloadPrimitives >>= reloadZedTui
handleEvent (VtyEvent (V.EvKey (V.KChar 's') []))  = saveZedTui >>= liftIO
handleEvent (VtyEvent (V.EvKey (V.KChar 'v') []))  = setFocus FocusVon
handleEvent (VtyEvent (V.EvKey (V.KChar 'b') []))  = setFocus FocusBis
handleEvent (VtyEvent (V.EvKey V.KEsc []))         = setFocus FocusListe
handleEvent (VtyEvent (V.EvKey (V.KChar 'q') []))  = halt
handleEvent (VtyEvent e)                           = handle e
--  | otherwise                                       = 
handleEvent _                                      = return ()
