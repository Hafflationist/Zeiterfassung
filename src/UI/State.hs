
module UI.State (Name, Focus(..), ZeiterfassungsdatenTUI(..)) where

import qualified Brick.Widgets.Edit as Edit
import qualified Brick.Widgets.List as L
import Data.DateTime
import Data.Sequence (Seq)
import Data.Text
import Data.Time.LocalTime

import Zeiterfassung.Data


data Focus = FocusListe | FocusVon | FocusBis deriving (Show, Eq)

type Name = Int

data ZeiterfassungsdatenTUI = ZeiterfassungsdatenTUI
  { zed                :: Zeiterfassungsdaten
  , rawDataGenericList :: L.GenericList Name Seq (Maybe DateTime, Maybe DateTime, DateTimeDiff)
  , editorVon          :: Edit.Editor Text Name
  , editorBis          :: Edit.Editor Text Name
  , lastFetch          :: LocalTime
  , focus              :: Focus 
  } deriving (Show)
