
module Pages.Todolist.Trace where

-------------------------------------------------------------------------------
import Prelude (class Show)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Minitools.Seqnum as Seqnum
-------------------------------------------------------------------------------
import Pages.Todolist.Action
import Pages.Todolist.StoredState

-------------------------------------------------------------------------------
data Trace
  = TraceAction (Seqnum.Seqnum "action") Action
  | RequestSave StoredState
derive instance genericTrace :: Generic Trace _
instance showTrace :: Show Trace where
  show = genericShow
