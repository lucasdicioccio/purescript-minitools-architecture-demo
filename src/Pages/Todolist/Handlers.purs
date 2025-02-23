
module Pages.Todolist.Handlers where

-------------------------------------------------------------------------------
import Prelude ((==),(/=),($),Unit,bind,discard,map,pure,unit)
import Effect.Class (liftEffect)
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.Array as Array
import Halogen as H
import Effect.Aff.Class (class MonadAff)
import Minitools.Seqnum as Seqnum
import Minitools.Tracer (Tracer, trace)

-------------------------------------------------------------------------------
import Pages.Todolist.Action
import Pages.Todolist.Base
import Pages.Todolist.State
import Pages.Todolist.Trace

-------------------------------------------------------------------------------

handleAction
  :: forall m slots output. (MonadAff m)
  => Tracer Trace
  -> Action
  -> H.HalogenM State Action slots output m Unit
handleAction tracer action =
  let
    requestSave :: H.HalogenM State Action slots output m Unit
    requestSave = do
      st <- H.get
      let savedState =
            { appSeqnum: st.seqnum
            , attachments: st.entities.attachments
            , notes: st.entities.notes
            , todos: st.entities.todos
            }
      trace tracer (RequestSave savedState)
  in
  case action of
    Initialize ->
      pure unit
    SetNewTodoText txt ->
      H.modify_ _ { ui { newTodoText = txt } }
    SetNewNoteTitle txt ->
      H.modify_ _ { ui { newNoteTitle = txt } }
    CreateTodo event -> do
      liftEffect $ traverse_ cancelEventPropagation event
      seqnum <- Seqnum.allocate
      let appendtodo st0 = Array.cons {seqnum, status: Todo, text: st0.ui.newTodoText} st0.entities.todos
      H.modify_ (\st0 -> st0 { entities { todos = appendtodo st0 }, ui { newTodoText = "" } })
      requestSave
    DeleteTodo seqnum -> do
      let filtertodo st0 = Array.filter (\x -> x.seqnum /= seqnum) st0.entities.todos
      let filterattachments st0 = Array.filter (\x -> x.todoSeqnum /= seqnum) st0.entities.attachments
      H.modify_ (\st0 -> st0 { entities { todos = filtertodo st0, attachments = filterattachments st0 } })
      requestSave
    CreateNote title -> do
      seqnum <- Seqnum.allocate
      let appendnote st0 = Array.cons {seqnum, title, content: ""} st0.entities.notes
      H.modify_ (\st0 -> st0 { entities { notes = appendnote st0 }, ui { newNoteTitle = "" } })
      requestSave
    CreateAttachedNote title todoSeqnum -> do
      seqnum <- Seqnum.allocate
      let appendnote st0 = Array.cons {seqnum, title, content: ""} st0.entities.notes
      let appendattachment st0 = Array.cons {todoSeqnum, noteSeqnum: seqnum} st0.entities.attachments
      H.modify_ (\st0 -> st0 { entities { notes = appendnote st0 , attachments = appendattachment st0 }, ui { newNoteTitle = "" } })
      requestSave
      handleAction tracer (StartEditingNote seqnum)
    DeleteNote seqnum -> do
      let filternote st0 = Array.filter (\x -> x.seqnum /= seqnum) st0.entities.notes
      let filterattachments st0 = Array.filter (\x -> x.noteSeqnum /= seqnum) st0.entities.attachments
      H.modify_ (\st0 -> st0 { entities { notes = filternote st0, attachments = filterattachments st0 } })
      requestSave
    StartEditingNote seqnum -> do
      let uiDefault = {noteSeqnum:seqnum,newTitle:"",newContent:""}
      let uiForNote st0 =
           map (\{title,content} -> {noteSeqnum:seqnum,newTitle:title,newContent:content})
           $ Array.find (\x -> x.seqnum == seqnum)
           $ st0.entities.notes
      let ui0 st0 =
            Maybe.fromMaybe uiDefault (uiForNote st0)
      let addUI st0 =
            Array.cons (ui0 st0)
            $ Array.filter (\x -> x.noteSeqnum /= seqnum)
            $ st0.ui.notes
      H.modify_ (\st0 -> st0 { ui { noteUnderEdition = Just seqnum, notes = addUI st0 } })
    StopEditingNote -> do
      H.modify_ (\st0 -> st0 { ui { noteUnderEdition = Nothing } })
    SaveEditedNote seqnum title content -> do
      let removeNoteUI st0 =
            Array.filter (\x -> x.noteSeqnum /= seqnum) 
            $ st0.ui.notes
      let updateNote st0 =
            Array.cons {seqnum,title,content}
            $ Array.filter (\x -> x.seqnum /= seqnum) 
            $ st0.entities.notes
      H.modify_ (\st0 -> st0 { ui { noteUnderEdition = Nothing , notes = removeNoteUI st0 }, entities { notes = updateNote st0 } })
      requestSave
    NoteUIAction seqnum (ChangeTitle title) -> do
      let updateOne ui
            | ui.noteSeqnum == seqnum = ui { newTitle = title }
            | true                    = ui
      let updateNoteUI st0 = map updateOne st0.ui.notes
      H.modify_ (\st0 -> st0 { ui { notes = updateNoteUI st0 } })
    NoteUIAction seqnum (ChangeContent content) -> do
      let updateOne ui
            | ui.noteSeqnum == seqnum = ui { newContent = content }
            | true                    = ui
      let updateNoteUI st0 = map updateOne st0.ui.notes
      H.modify_ (\st0 -> st0 { ui { notes = updateNoteUI st0 } })
    TodoAction seqnum (UpdateStatus st) -> do
      let mod1todo todo 
           | todo.seqnum == seqnum = todo { status = st }
           | true                  = todo 
      let modifytodos st0 =
            map mod1todo st0.entities.todos
      H.modify_ (\st0 -> st0 { entities { todos = modifytodos st0 } })
      requestSave
    ZoomOnTodo seqnum -> do
      H.modify_ (\st0 -> st0 { ui { todoUnderMicroscope = Just seqnum } })
    ZoomOnTodoNotes seqnum -> do
      H.modify_ (\st0 -> st0 { ui { todoNotesUnderMicroscope = Just seqnum } })
    UnZoom -> do
      H.modify_ (\st0 -> st0 { ui { todoUnderMicroscope = Nothing, todoNotesUnderMicroscope = Nothing } })
    SetAttachmentSearchString str -> do
      H.modify_ (\st0 -> st0 { ui { attachmentSearchString = str } })
    AddAttachment todoSeqnum noteSeqnum -> do
      let insertAttachment st0 =
            Array.cons {todoSeqnum, noteSeqnum} st0.entities.attachments
      H.modify_ (\st0 -> st0 { ui { attachmentSearchString = "" }, entities { attachments = insertAttachment st0 } })
      requestSave
    RemoveAttachment todoSeqnum noteSeqnum -> do
      let removeAttachment st0 =
            Array.filter (\x -> x /= {todoSeqnum, noteSeqnum}) st0.entities.attachments
      H.modify_ (\st0 -> st0 { entities { attachments = removeAttachment st0 } })
      requestSave
