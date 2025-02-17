
module Pages.Todolist.Base where

--------------------------------------------------------------------------------
import Prelude (class Show)
import Data.Generic.Rep (class Generic)
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Minitools.Seqnum (Seqnum)

--------------------------------------------------------------------------------
data TodoStatus
  = Todo
  | Doing
  | Done
derive instance genericTodoStatus :: Generic TodoStatus _
instance showTodoStatus :: Show TodoStatus where
  show Todo = "Todo"
  show Doing = "Doing"
  show Done = "Done"
instance encodeJsonTodoStatus :: EncodeJson TodoStatus where
  encodeJson a = genericEncodeJson a
instance decodeJsonTodoStatus :: DecodeJson TodoStatus where
  decodeJson a = genericDecodeJson a

isOpen :: TodoStatus -> Boolean
isOpen Todo = true
isOpen Doing = true
isOpen Done = false
 
type Todo =
  { seqnum :: Seqnum "todo"
  , text :: String
  , status :: TodoStatus
  }

type Note =
  { seqnum :: Seqnum "note"
  , title :: String
  , content :: String
  }

type NoteUI =
  { noteSeqnum :: Seqnum "note"
  , newTitle :: String
  , newContent :: String
  }

type Attachment =
  { noteSeqnum :: Seqnum "note"
  , todoSeqnum :: Seqnum "todo"
  }

