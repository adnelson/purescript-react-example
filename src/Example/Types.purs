module Example.Types where

import Prelude

import Data.Generic.Rep (class Generic) as G
import Data.Generic.Rep.Show as GShow
import Data.Newtype (class Newtype)

data TodoStatus
  = TodoPending
  | TodoDone
  | TodoCleared

derive instance genericTodoStatus :: G.Generic TodoStatus _
derive instance eqTodoStatus :: Eq TodoStatus
derive instance ordTodoStatus :: Ord TodoStatus
instance showTodoStatus :: Show TodoStatus where show = GShow.genericShow

newtype Todo
  = Todo { text :: String
         , status :: TodoStatus
         }

derive instance genericTodo :: G.Generic Todo _

derive instance eqTodo :: Eq Todo
derive instance ordTodo :: Ord Todo
instance showTodo :: Show Todo where show = GShow.genericShow

derive instance newtypeTodo :: Newtype Todo _
