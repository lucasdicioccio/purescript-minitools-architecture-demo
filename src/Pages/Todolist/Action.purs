
module Pages.Todolist.Action where

-------------------------------------------------------------------------------
import Prelude (class Show)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Minitools.Seqnum (Seqnum)

-------------------------------------------------------------------------------
import Pages.Todolist.Base
import Pages.Todolist.State

-------------------------------------------------------------------------------
data TodoAction
  = UpdateStatus TodoStatus
derive instance genericTodoAction :: Generic TodoAction _
instance showTodoAction :: Show TodoAction where
  show = genericShow

data NoteUIAction
  = ChangeTitle String
  | ChangeContent String
derive instance genericNoteUIAction :: Generic NoteUIAction _
instance showNoteUIAction :: Show NoteUIAction where
  show = genericShow

data Action
  = Initialize
  | SetNewTodoText String
  | CreateTodo
  | TodoAction (Seqnum "todo") TodoAction
  | DeleteTodo (Seqnum "todo")
  | SetNewNoteTitle String
  | CreateNote String
  | CreateAttachedNote String (Seqnum "todo")
  | NoteUIAction (Seqnum "note") NoteUIAction
  | StartEditingNote (Seqnum "note")
  | SaveEditedNote (Seqnum "note") String String
  | StopEditingNote
  | DeleteNote (Seqnum "note")
  | ZoomOnTodo (Seqnum "todo")
  | ZoomOnTodoNotes (Seqnum "todo")
  | UnZoom
  | AddAttachment (Seqnum "todo") (Seqnum "note")
  | RemoveAttachment (Seqnum "todo") (Seqnum "note")
  | SetAttachmentSearchString String
derive instance genericAction :: Generic Action _
instance showAction :: Show Action where
  show = genericShow
