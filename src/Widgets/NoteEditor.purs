
module Widgets.NoteEditor where

--------------------------------------------------------------------------------
import Prelude (($), (==), const)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Minitools.Bricks.ActionButton as ActionButton

--------------------------------------------------------------------------------
import Utils (reportKey)

type Props a =
  { title :: String
  , content :: String
  , onSave :: a
  , onClose :: a
  , onDelete :: a
  , onTitleChange :: String -> a
  , onContentChange :: String -> a
  }

render
  :: forall slots action m. MonadAff m
  => Props action
  -> H.ComponentHTML action slots m
render props =
  HH.div
  [ HP.classes
    [ HH.ClassName "modal"
    , HH.ClassName "is-active"
    ]
  ]
  [ HH.div
    [ HP.class_ $ HH.ClassName "modal-background"
    , HE.onClick (const props.onClose)
    , reportKey "modal-background"
    ]
    [
    ]
  , HH.div
    [ HP.class_ $ HH.ClassName "modal-card"
    ]
    [ HH.header
      [ HP.class_ $ HH.ClassName "modal-card-head"
      ]
      [ HH.p
        [ HP.class_ $ HH.ClassName "modal-card-title"
        ]
        [ HH.text "Editing note"
        ]
      ]
    , HH.section
      [ HP.class_ $ HH.ClassName "modal-card-body"
      , reportKey "modal-card-body"
      ]
      [ HH.div
        [ HP.class_ $ HH.ClassName "field"
        ]
        [ HH.label
          [ HP.class_ $ HH.ClassName "label"
          ]
          [ HH.text "title"
          ]
        , HH.div
          [ HP.class_ $ HH.ClassName "control"
          ]
          [ HH.input
            [ HP.class_ $ HH.ClassName "input"
            , HP.value props.title
            , HE.onValueChange props.onTitleChange
            ]
          ]
        ]
      , HH.div
        [ HP.class_ $ HH.ClassName "field"
        ]
        [ HH.label
          [ HP.class_ $ HH.ClassName "label"
          ]
          [ HH.text "content"
          ]
        , HH.div
          [ HP.class_ $ HH.ClassName "control"
          ]
          [ HH.textarea
            [ HP.class_ $ HH.ClassName "input"
            , HP.rows 10
            , HP.value props.content
            , HE.onValueChange props.onContentChange
            ]
          ]
        ]
      ]
    , HH.footer
      [ HP.class_ $ HH.ClassName "modal-card-foot"
      ]
      [ HH.div
        [ HP.classes [ HH.ClassName "buttons" ]
        , reportKey "modal-card-buttons"
        ]
        [ ActionButton.render2
          { text: "save"
          , classes: [ HH.ClassName "button", HH.ClassName "is-primary" ]
          , info: "save"
          , action: props.onSave
          , disabled: props.title == ""
          }
        , ActionButton.render
          { text: "close"
          , info: "discard changes"
          , action: props.onSave
          , disabled: false
          }
        , ActionButton.render2
          { text: "delete"
          , classes: [ HH.ClassName "button", HH.ClassName "is-danger" ]
          , info: "delete note"
          , action: props.onDelete
          , disabled: false
          }
        ]
      ]
    ]
  ]
