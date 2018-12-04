module Example.Types where

{-
So, given a set of props P, you can create a react class C:

myClass :: React.ReactClass P
myClass = React.component "MyClass" $ \this -> pure {state, render}

Where state is the initial state and render is markup.


Then, once you've defined the class, you can create instances of it with createLeafElement to use in a render method, passing it an instance of Props.

myOtherClass = React.component "MyOtherClass" $ \this -> pure {
  state: {},
  render: do
    props <- doSomethingToGetProps
    props2 <- doSomethingElseToGetProps
    pure $ DOM.div [] [
      React.createLeafElement myClass props,
      React.createLeafElement myClass props2
      ]
  }

Finally, for actually plugging your app into the page, you use `ReactDOM.render`

main = do
  element <- getTheElementToMount
  ReactDOM.render (React.createLeafElement myOtherClass {}) element
-}

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
