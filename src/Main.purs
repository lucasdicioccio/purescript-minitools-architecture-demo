module Main where

-------------------------------------------------------------------------------
import Prelude
import Data.Argonaut.Core as Json
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Decode (fromJsonString)
import Data.Argonaut.Encode (encodeJson)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..),fromMaybe)
import Data.Traversable (traverse)
import Data.Tuple as Tuple
import Effect.Aff.Class (class MonadAff)
import Effect.Console as Console
import Effect (Effect)
import Foreign.Object as Object
import Halogen.Aff (awaitBody, selectElement, runHalogenAff)
import Halogen as H
import Halogen.VDom.Driver (runUI)
import Minitools.PageEvents as PageEvents
import Minitools.Reports as Reports
import Minitools.Tracer as Minitools
import Unsafe.Coerce as Unsafe
import Web.DOM.ParentNode (QuerySelector(..))
import Web.HTML (window)
import Web.HTML.Window (Window, localStorage)
import Web.Storage.Storage (Storage, getItem, setItem)

-------------------------------------------------------------------------------
import Pages.Todolist as Todolist
import Pages.Todolist.Action as Todolist
import Pages.Todolist.Trace as Todolist
import Pages.Todolist.StoredState as Todolist

-------------------------------------------------------------------------------

type Params =
  { recording :: Reports.Recording
  , pageEvents :: PageEvents.Config
  }

component
  :: forall query m. MonadAff m
  => Params
  -> (Reports.AnonymousPush -> Effect Unit)
  -> Storage
  -> H.Component query Todolist.PageInput Void m
component params pushReport storage =
    Todolist.page {tracer}
  where
    tracer :: Todolist.Trace -> Effect Unit
    tracer = Minitools.traceBoth traceSave (Minitools.traceBoth traceConsole traceReports)

    traceSave :: Todolist.Trace -> Effect Unit
    traceSave (Todolist.RequestSave state) = do
      setItem storageKey (stringify $ encodeJson state) storage
    traceSave (Todolist.TraceAction _ _) = do
      pure unit

    traceConsole :: Todolist.Trace -> Effect Unit
    traceConsole item = 
      Console.log (show item)

    traceReports :: Todolist.Trace -> Effect Unit
    traceReports (Todolist.RequestSave ev) = 
      pure unit
    traceReports (Todolist.TraceAction _ ev) = 
      for_ (mappedEvent ev) $ \e ->
        pushReport {e}
          where
            mappedEvent =
              case _ of
                (Todolist.Initialize) ->
                   Just (key "init")
                (Todolist.CreateTodo Nothing) ->
                   Just
                   $ object
                     [ kvStr "key" "create-todo"
                     , kvStr "style" "direct"
                     ]
                (Todolist.CreateTodo (Just _)) ->
                   Just
                   $ object
                     [ kvStr "key" "create-todo"
                     , kvStr "style" "via-form"
                     ]
                (Todolist.DeleteTodo _) ->
                   Just (key "delete-todo")
                (Todolist.CreateNote _) ->
                   Just (key "create-note")
                (Todolist.DeleteNote _) ->
                   Just (key "delete-note")
                (Todolist.CreateAttachedNote _ _) ->
                   Just (key "create-attached-note")
                (Todolist.StartEditingNote _) ->
                   Just (key "edit-note")
                (Todolist.SaveEditedNote _ _ _) ->
                   Just (key "save-note")
                (Todolist.StopEditingNote) ->
                   Just (key "quit-edit-note")
                (Todolist.ZoomOnTodo _) ->
                   Just (key "zoom-on-todo")
                (Todolist.ZoomOnTodoNotes _) ->
                   Just (key "zoom-on-todo-notes")
                (Todolist.UnZoom) ->
                   Just (key "unzoom")
                (Todolist.AddAttachment _ _) ->
                   Just (key "attach-note")
                (Todolist.RemoveAttachment _ _) ->
                   Just (key "dettach-note")
                (Todolist.TodoAction _ (Todolist.UpdateStatus s)) ->
                   Just
                   $ object
                     [ kvStr "key" "effect"
                     , kvStr "status" (show s)
                     ]
                (Todolist.NoteUIAction _ (Todolist.ChangeTitle s)) ->
                   Nothing
                (Todolist.NoteUIAction _ (Todolist.ChangeContent s)) ->
                   Nothing
                (Todolist.SetAttachmentSearchString _) ->
                   Nothing
                (Todolist.SetNewTodoText _) ->
                   Nothing
                (Todolist.SetNewNoteTitle _) ->
                   Nothing

    keyValStr key str =
      Json.jsonSingletonObject key (Json.fromString str)
    kvStr key str =
      Tuple.Tuple key (Json.fromString str)
    key str =
      keyValStr "key" str
    object = Json.fromObject <<< Object.fromFoldable

main :: Effect Unit
main = do
    w <- window
    params <- initWithDefaultParams
    doRun w params
  where
    doRun :: Window -> Params -> Effect Unit
    doRun w params = do
      -- initialize analytics
      pusher <- Reports.init params.recording
      PageEvents.tapElement
        (pusher <<< mappedAnnotation)
        params.pageEvents
        (Unsafe.unsafeCoerce w)

      -- initialize storage
      storage <- localStorage w
      mEncodedState <- getItem storageKey storage
      let storedState =
            case map fromJsonString mEncodedState of
              (Just (Right state)) -> state
              _ -> Todolist.storedState0

      -- run whole component
      runHalogenAff do
        body <- awaitBody
        spaElem <- selectElement (QuerySelector "#spa")
        let tgt = fromMaybe body spaElem
        runUI (component params pusher storage) {storedState} tgt

    initWithDefaultParams :: Effect Params
    initWithDefaultParams = do
      refreshString <- Reports.generateRecordingRefresh
      let recording = { baseUrl: "https://dicioccio.fr/api/blog_reports/minitools_demo_reports" , refreshString}
      pure {recording, pageEvents: PageEvents.defaultConfig}

-------------------------------------------------------------------------------
mappedAnnotation :: PageEvents.Annotation -> Reports.AnonymousPush
mappedAnnotation ann = {e}
  where
    e :: Json.Json
    e = Json.fromObject
        $ Object.fromFoldable
        [ kvStr "key" "dom-event"
        , kvStr "elem:id" ann.elementID
        , kvStr "ev:t" ann.eventType
        , Tuple.Tuple "attrs:list" (Json.fromArray $ map Json.fromString attrs)
        , Tuple.Tuple "kvs" (Json.fromArray $ map (Json.fromObject <<< Object.fromFoldable <<< map kvTuple) kvs)
        ]

    attrs = map _.k ann.attributes
    kvs = map _.kvs ann.attributes

    kvStr key str =
      Tuple.Tuple key (Json.fromString str)
    kvTuple (Tuple.Tuple key str) = kvStr key str

storageKey :: String
storageKey = "minitools-demo-todo-storage-key" 
