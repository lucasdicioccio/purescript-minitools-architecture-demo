
module Pages.Todolist.StoredState where

--------------------------------------------------------------------------------
import Data.Monoid (mempty)
import Minitools.Seqnum (Seqnum)
import Minitools.Seqnum as Seqnum

--------------------------------------------------------------------------------
import Pages.Todolist.Base

--------------------------------------------------------------------------------
type StoredState =
  { appSeqnum :: Seqnum "state"
  , todos :: Array Todo
  , notes :: Array Note
  , attachments :: Array Attachment
  }

storedState0 :: StoredState
storedState0 =
  { appSeqnum: Seqnum.Seqnum 0
  , todos: mempty
  , notes: mempty
  , attachments: mempty
  }
