
module Pages.Todolist.State where

-------------------------------------------------------------------------------
import Data.Maybe (Maybe)
import Minitools.Seqnum (Seqnum)

-------------------------------------------------------------------------------
import Pages.Todolist.Base

-------------------------------------------------------------------------------

type State =
  { seqnum :: Seqnum "state"
  , entities :: 
    { todos :: Array Todo
    , notes :: Array Note
    , attachments :: Array Attachment
    }
  , ui :: 
    { notes :: Array NoteUI
    , newNoteTitle :: String
    , newTodoText :: String
    , noteUnderEdition :: Maybe (Seqnum "note")
    , todoUnderMicroscope :: Maybe (Seqnum "todo")
    , todoNotesUnderMicroscope :: Maybe (Seqnum "todo")
    , attachmentSearchString :: String
    }
  }
