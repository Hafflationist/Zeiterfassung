{-# language TemplateHaskell #-}

module UI.State (
    Name, Focus(..), ZeiterfassungsdatenTUI(..), 
    zed,
    rawDataGenericList,
    editorVon,
    lastFetch,
    focus,
    editorBis
    ) where

import qualified Brick.Widgets.Edit as Edit
import qualified Brick.Widgets.List as L
import Data.Sequence (Seq)
import Data.Text
import Data.Time.LocalTime

import Lens.Micro.Mtl (use, (.=))
import Lens.Micro.TH (makeLenses)

import Zeiterfassung.Data


data Focus = FocusListe | FocusVon | FocusBis deriving (Show, Eq)

type Name = Int

data ZeiterfassungsdatenTUI = ZeiterfassungsdatenTUI
  { _zed                :: Zeiterfassungsdaten
  , _rawDataGenericList :: L.GenericList Name Seq (Maybe LocalTime, Maybe LocalTime, DateTimeDiff)
  , _editorVon          :: Edit.Editor Text Name
  , _lastFetch          :: LocalTime
  , _focus              :: Focus 
  , _editorBis          :: Edit.Editor Text Name
  } deriving (Show)

makeLenses ''ZeiterfassungsdatenTUI
