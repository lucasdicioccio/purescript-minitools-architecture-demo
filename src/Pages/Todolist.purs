
module Pages.Todolist where

-------------------------------------------------------------------------------
import Prelude (($),(<>),(==),(<<<),(&&),bind,const,discard,map,show)
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.List as List
import Data.Monoid (mempty)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Effect.Aff.Class (class MonadAff)
import Minitools.Tracer (Tracer, trace)
import Minitools.Seqnum (Seqnum)
import Minitools.Seqnum as Seqnum
import Minitools.Bricks.ActionButton as ActionButton
import Minitools.Bricks.Emojis as Emojis
import Minitools.Bricks.Message as Message
import Minitools.Bricks.SetSearchSelector as SetSearchSelector
import Minitools.Bricks.Table as Table

-------------------------------------------------------------------------------
import Pages.Todolist.Action (Action(..),TodoAction(..),NoteUIAction(..))
import Pages.Todolist.Base (Note, isOpen)
import Pages.Todolist.StoredState (StoredState)
import Pages.Todolist.Trace (Trace(..))
import Pages.Todolist.State (State)
import Pages.Todolist.Handlers as Handlers
import Pages.Todolist.Base (TodoStatus(..))
import Utils (reportKey,stringMatchMulti)
import Widgets.NoteEditor as NoteEditor

--------------------------------------------------------------------------------
state0 :: StoredState -> State
state0 st0@{todos,notes,attachments} =
  { seqnum: st0.appSeqnum
  , entities:
    { todos
    , notes
    , attachments
    }
  , ui:
    { notes: mempty
    , newNoteTitle: ""
    , newTodoText: ""
    , noteUnderEdition: Nothing
    , todoUnderMicroscope: Nothing
    , todoNotesUnderMicroscope: Nothing
    , attachmentSearchString: ""
    }
  }

type PageInput =
  { storedState :: StoredState
  }

type Props =
  { tracer :: Tracer Trace
  }

page
  :: forall slots query output m. MonadAff m
  => Props
  -> H.Component query PageInput output m
page { tracer } =
    H.mkComponent
      { initialState: \i -> state0 i.storedState
      , render
      , eval: H.mkEval $ H.defaultEval
          { handleAction = \act -> do
              seqnum <- Seqnum.allocate
              H.liftEffect (trace tracer $ TraceAction seqnum act)
              Handlers.handleAction tracer act
          , initialize = Just Initialize
          }
      }
  where
    render :: State -> H.ComponentHTML Action slots m
    render state =
      HH.div_
      [ HH.div
        [ HP.class_ (HH.ClassName "section")
        ]
        [ HH.div
          [ HP.classes
            [ HH.ClassName "container"
            , HH.ClassName "columns"
            ]
          , reportKey "page"
          ]
          [ HH.div
            [ HP.classes [ HH.ClassName "column", HH.ClassName "content" ]
            , reportKey "debug-zone"
            ]
            [ HH.h1_ [ HH.text "about"  ]
            , render_about
            , HH.h2_ [ HH.text "credits"  ]
            , render_credits
            ]
          , HH.div
            [ HP.classes [ HH.ClassName "column", HH.ClassName "content" ]
            , reportKey "debug-zone"
            ]
            [ HH.h4_ [ HH.text "state dump"  ]
            , render_debug state
            ]
          , HH.div
            [ HP.classes [ HH.ClassName "column", HH.ClassName "content" ]
            ]
            [ HH.h1_ [ HH.text "new todo"  ]
            , render_newTodo state
            , render_todos state
            ]
          , render_todoNotes state
          , HH.div
            [ HP.classes [ HH.ClassName "column", HH.ClassName "content" ]
            ]
            [ HH.h1_ [ HH.text "new note"  ]
            , render_newNote state
            , render_notes state
            ]
          ]
        ]
      , render_modalEditNote state
      ]

    render_todoNotes state =
      let
        attachmentsForTodo todoSeqnum =
          Array.filter (\x -> todoSeqnum == x.todoSeqnum) state.entities.attachments
        notesForTodo todoSeqnum =
          let
            notesSeqnums = map _.noteSeqnum (attachmentsForTodo todoSeqnum)
          in
          Array.filter (\x -> x.seqnum `Array.elem` notesSeqnums) state.entities.notes
      in
      case state.ui.todoNotesUnderMicroscope of
        Nothing -> HH.text ""
        Just todoSeqnum ->
          HH.div
          [ HP.classes [ HH.ClassName "column", HH.ClassName "content" ]
          ]
          [ HH.h1_ [ HH.text "notes for todo"  ]
          , HH.div
            [ reportKey "notes"
            ]
            $ map (render_notes_note state) (notesForTodo todoSeqnum)
          ]

    render_newTodo state =
      HH.div
      [ HP.classes [ HH.ClassName "field", HH.ClassName "has-addons" ]
      , reportKey "new-todo"
      ]
      [ HH.div
        [ HP.class_ (HH.ClassName "control")
        ]
        [ HH.input
          [ HP.class_ (HH.ClassName "input")
          , HP.placeholder "cheer for Lucas' blog on social media"
          , HE.onValueInput SetNewTodoText
          ]
        ]
      , HH.div
        [ HP.class_ (HH.ClassName "control")
        ]
        [ ActionButton.render
          { text: "add"
          , info: "insert new todo item"
          , disabled: state.ui.newTodoText == ""
          , action: CreateTodo
          }
        ]
      ]

    render_todos state =
       let
         statusColumn =
           { cell: render_todos_statusCell state
           , colgrp: []
           , column: render_todos_statusColumn state
           , name: "status"
           }
         textColumn =
           { cell: render_todos_textCell state
           , colgrp: []
           , column: HH.text ""
           , name: "item"
           }
         notesColumn =
           { cell: render_todos_notesCell state
           , colgrp: []
           , column: HH.text ""
           , name: "notes"
           }
         actionsColumn =
           { cell: render_todos_actionsCell state
           , colgrp: []
           , column: HH.text ""
           , name: "actions"
           }
       in
       Table.render
       { items: state.entities.todos
       , rowClass: const mempty
       , template:
         [ statusColumn
         , textColumn
         , notesColumn
         , actionsColumn
         ]
       }

    render_todos_statusColumn state =
      let
        open =
          Array.length
          $ Array.filter (\todo -> isOpen todo.status)
          $ state.entities.todos
        total = Array.length state.entities.todos
      in
      HH.span
      [ HP.class_ (HH.ClassName "tag")
      ]
      [ HH.text $ show open
      , HH.text "/"
      , HH.text $ show total
      ]

    render_todos_statusCell state todo =
      let
        btn text nextStatus =
          ActionButton.render
          { text
          , disabled: false
          , info: "cycle to next state"
          , action: TodoAction todo.seqnum (UpdateStatus nextStatus)
          }
      in
      case todo.status of
        Todo -> btn Emojis.boxUnChecked Doing
        Doing -> btn Emojis.handPointingRight Done
        Done -> btn Emojis.boxChecked Todo

    render_todos_textCell state todo =
      let
        zoomed = state.ui.todoUnderMicroscope == Just todo.seqnum
        clickEvent =
          if zoomed
          then UnZoom
          else ZoomOnTodo todo.seqnum
        style =
          case todo.status of
            Todo -> HH.ClassName "has-text-weight-normal"
            Doing -> HH.ClassName "has-text-weight-bold"
            Done -> HH.ClassName "has-text-weight-light"
      in
      HH.div
      [ reportKey "title-cell"
      ]
      [ HH.p
        [ HP.classes [ HH.ClassName "is-clickable", style ]
        , HE.onClick (const clickEvent)
        ]
        [ HH.text todo.text
        ]
      , if zoomed
        then
          HH.div
          [ reportKey "todo-text"
          , reportKey "search notes"
          ]
          [ render_todos_textCell_searchNotes state todo
          ]
        else
          HH.text ""
      ]

    render_todos_textCell_searchNotes state todo =
      let
        matchItem str note = stringMatchMulti str note.title
        renderItem note = HH.text note.title
        activeItem note =
          Array.any
            (\{noteSeqnum,todoSeqnum} -> noteSeqnum == note.seqnum && todoSeqnum  == todo.seqnum)
            state.entities.attachments
        onChecked note isSet =
          if isSet
          then AddAttachment todo.seqnum note.seqnum
          else RemoveAttachment todo.seqnum note.seqnum
      in
      SetSearchSelector.render
      { title: "notes"
      , items: List.fromFoldable state.entities.notes
      , searchString: state.ui.attachmentSearchString
      , matchItem
      , renderItem
      , activeItem
      , onSearch: SetAttachmentSearchString
      , onChecked
      , menuClass: ""
      , menuIcon: HH.text Emojis.blueDot
      , renderNotFound:
         HH.div_
         [ HH.text "no such item"
         , ActionButton.render2
           { text: "create"
           , classes: [ HH.ClassName "button", HH.ClassName "is-primary", HH.ClassName "is-small" ]
           , info: "create new note"
           , disabled: state.ui.attachmentSearchString == ""
           , action: CreateAttachedNote state.ui.attachmentSearchString todo.seqnum
           }
         ]
      }

    render_todos_notesCell state todo =
      let
        notesForTodo =
          Array.filter (\{todoSeqnum} -> todoSeqnum == todo.seqnum) state.entities.attachments
        zoomed = state.ui.todoNotesUnderMicroscope == Just todo.seqnum
        clickEvent =
          if zoomed
          then UnZoom
          else ZoomOnTodoNotes todo.seqnum
      in
      HH.div
      [ HP.class_ $ HH.ClassName "is-clickable"
      , reportKey "notes-count"
      , HE.onClick (const clickEvent)
      ]
      [ HH.strong
        [ if zoomed
          then HP.style "background: orange; border-radius: 6px; padding: 8px"
          else HP.style ""
        ]
        [ HH.text $ show $ Array.length notesForTodo
        ]
      ]

    render_todos_actionsCell state todo =
      ActionButton.render
      { text: Emojis.trashBin
      , info: "destroy this item"
      , action: DeleteTodo todo.seqnum
      , disabled: false
      }

    render_newNote state =
      HH.div
      [ HP.classes [ HH.ClassName "field", HH.ClassName "has-addons" ]
      , reportKey "new-note"
      ]
      [ HH.div
        [ HP.class_ (HH.ClassName "control")
        ]
        [ HH.input
          [ HP.class_ (HH.ClassName "input")
          , HP.placeholder "some note"
          , HE.onValueChange SetNewNoteTitle
          , HP.value state.ui.newNoteTitle
          ]
        ]
      , HH.div
        [ HP.class_ (HH.ClassName "control")
        ]
        [ ActionButton.render
          { text: "add"
          , info: "insert new note"
          , disabled: state.ui.newNoteTitle == ""
          , action: CreateNote state.ui.newNoteTitle
          }
        ]
      ]

    render_notes state =
      HH.div
      [ reportKey "notes"
      ]
      $ map (render_notes_note state) state.entities.notes

    render_notes_note state note =
      HH.div
      [ HP.classes [ HH.ClassName "mb-2" ]
      , reportKey "note"
      ]
      [ HH.div
        [ HP.class_ $ HH.ClassName "is-clickable"
        , HE.onClick (const $ StartEditingNote note.seqnum)
        ]
        [ Message.render
          { level: Message.Success
          , main: note.title
          , details:
              if note.content == ""
              then Message.NoDetails
              else Message.ExtraDetails note.content
          }
        ]
      ]

    render_debug state =
      let
         bubble txt =
           HH.span
           [ HP.classes [ HH.ClassName "tag", HH.ClassName "mr-2" ]
           ]
           [ HH.text txt
           ]
      in
      HH.div_
      [ HH.h6_ [ HH.text "app-seqnum"  ]
      , HH.div_
        [ bubble $ show state.seqnum
        ]
      , HH.h6_ [ HH.text "todos-seqnums"  ]
      , HH.div_
        $ map (\todo -> bubble $ show todo.seqnum)
        $ state.entities.todos
      , HH.h6_ [ HH.text "notes-seqnums"  ]
      , HH.div_
        $ map (\note -> bubble $ show note.seqnum)
        $ state.entities.notes
      , HH.h6_ [ HH.text "todo-note attachments"  ]
      , HH.div_
        $ map (\x -> bubble $ show x.noteSeqnum <> " | " <> show x.todoSeqnum)
        $ state.entities.attachments
      , HH.h6_ [ HH.text "whole state"  ]
      , HH.p_
        [ HH.text $ show state
        ]
      ]

    render_modalEditNote state =
      let
        note :: Maybe Note
        note = do
          seqnum <- state.ui.noteUnderEdition
          Array.find (\x -> x.seqnum == seqnum) state.entities.notes
      in
      case note of
        Nothing -> HH.text ""
        (Just n) -> render_modalEditNote_note state n

    render_modalEditNote_note state note =
      let
        foundUI = Array.find (\x -> x.noteSeqnum == note.seqnum) state.ui.notes
      in
      case foundUI of
        Nothing ->
          Message.render
          { level: Message.Error
          , main: "UI state not found"
          , details:
              Message.ExtraDetails "If we encounter this message, it means our handler-code has forgotten to initialize a UI state. A more resilient approach would have been to default to some UI object here."
          }
        Just ui ->
          NoteEditor.render
          { title: ui.newTitle
          , content: ui.newContent
          , onClose: StopEditingNote
          , onSave: SaveEditedNote note.seqnum ui.newTitle ui.newContent
          , onDelete: DeleteNote note.seqnum
          , onTitleChange: NoteUIAction note.seqnum <<< ChangeTitle
          , onContentChange: NoteUIAction note.seqnum <<< ChangeContent
          }

render_about =
  HH.div
  [ reportKey "about"
  ]
  [ HH.p_ 
    [ HH.text "This page demonstrates the architecture patterns described in my "
    , HH.a
      [ HP.href "https://dicioccio.fr/topics/purescript-halogen-architecture.html"
      ]
      [ HH.text "series of blogposts about PureScript/Halogen architecture."
      ]
    ]
  , HH.p_ 
    [ HH.text "The source code for this page is available "
    , HH.a
      [ HP.href "https://github.com/lucasdicioccio/purescript-minitools-architecture-demo"
      ]
      [ HH.text "on GitHub."
      ]
    ]
  , HH.p_ 
    [ HH.text "This page is a somewhat standard TODO-list. "
    , HH.text "Each ", HH.em_ [ HH.text "Todo" ], HH.text " may have ", HH.em_ [ HH.text "Notes" ], HH.text " attached in an m:n relationship."
    , HH.text "Storage happens on the browsers' locale-storage. "
    , HH.text "Analytics report anonymously and with only action type digests to my blog. "
    ]
  , HH.p_ 
    [ HH.text "Consider this page as a skeleton to improve upon. "
    , HH.text "I would like to have individual commits starting from this base for showing how to evolve such todo-app. "
    , HH.text "(A) it is interesting to add some filter/sorter on the table like I did in PostgREST-Table. "
    , HH.text "(B) we need to modify the Action and Handlers to support a tutorial or insert-data mode. "
    , HH.text "(C) we need some slight modification to the HTML layout so that the 'add buttons' are bound to onSubmit rather than onClick. "
    , HH.text "(D) add some sum-type to specialize notes (e.g., if a note is an address we may link to an online-map service)."
    ]
  , HH.p_ 
    [ HH.text "Contributions are welcome. "
    , HH.text "Feel free to submit a code change or even just show-off a diff (include a link to your profile in the credits below). "
    , HH.text "Although the goal of this demo is not to make something pretty, if you wish to improve on the general style."
    ]
  ]

render_credits =
  HH.ul
  [ reportKey "credits"
  ]
  [ HH.li_ 
    [ HH.a
      [ HP.href "https://dicioccio.fr/"
      ]
      [ HH.text "Lucas DiCioccio"
      ]
    ]
  , HH.li_ 
    [ HH.a
      [ HP.href "https://purescript-halogen.github.io/purescript-halogen/"
      ]
      [ HH.text "Halogen"
      ]
    ]
  , HH.li_ 
    [ HH.a
      [ HP.href "https://bulma.io/"
      ]
      [ HH.text "Bulma"
      ]
    ]
  ]
