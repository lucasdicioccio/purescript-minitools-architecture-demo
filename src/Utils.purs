
module Utils where

--------------------------------------------------------------------------------
import Prelude ((==),($),(<>),map)
import Data.List as List
import Data.Tuple (Tuple(..))
import Halogen as H
import Halogen.HTML.Properties as HP
import Data.String.Common (toLower) as Strings
import Data.String.CodeUnits (contains) as Strings
import Data.String.Extra (words) as Strings
import Data.String.Pattern (Pattern(..)) as Strings

--------------------------------------------------------------------------------

reportKey :: forall r i. String -> HP.IProp r i
reportKey = HP.attr (H.AttrName "data-ev")

reportKeyVals :: forall r i. Array (Tuple String String) -> HP.IProp r i
reportKeyVals items = HP.attr (H.AttrName "data-kv") str
  where
    str :: String
    str = List.intercalate ";" (map pair items)

    pair :: Tuple String String -> String
    pair  (Tuple k v) = k <> "=" <> v

kv :: String -> String -> Tuple String String
kv k v = Tuple k v

--------------------------------------------------------------------------------

stringMatchMulti :: String -> String -> Boolean
stringMatchMulti query candidate =
  if query == ""
  then true
  else List.all (\sub -> stringMatchSingle sub candidate) $ Strings.words query

stringMatchSingle :: String -> String -> Boolean
stringMatchSingle query candidate =
  if query == ""
  then true
  else Strings.contains (Strings.Pattern lquery) lcandidate
  where
    lquery = Strings.toLower query
    lcandidate = Strings.toLower candidate
