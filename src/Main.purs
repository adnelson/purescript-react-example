module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Effect.Random as Random

import Data.Tuple (Tuple(..), snd)
import Data.Array (snoc, modifyAt, elemIndex, cons, length, sortBy, zip, fromFoldable)
import Data.List.Lazy (replicateM)
import Data.Maybe (Maybe(..), fromJust, fromMaybe)

import Web.HTML.HTMLDocument (toNonElementParentNode) as DOM
import Web.DOM.NonElementParentNode (getElementById) as DOM
import Web.HTML (window) as DOM
import Web.HTML.Window (document) as DOM
import React.DOM as DOM
import React.DOM.Props as Props

import Partial.Unsafe (unsafePartial)

import React as React
import ReactDOM as ReactDOM

import Game.Set as Set
import Example.TodoList (todoListClass)
import Example.Types (Todo(..), TodoStatus(..))

shuffle :: forall e a. Array a -> Effect (Array a)
shuffle xs = do
  let len = length xs
  randoms <- fromFoldable <$> replicateM len (Random.randomInt 0 (len - 1))
  pure $ map snd $ sortBy compareFst $ zip randoms xs
  where compareFst (Tuple a _) (Tuple b _) = compare a b

main :: Effect Unit
main = void $ do
  document <- DOM.document =<< DOM.window
  let node = DOM.toNonElementParentNode document
  element <- DOM.getElementById "example" node
  let element' = unsafePartial (fromJust element)
  ReactDOM.render (React.createLeafElement mainClass { }) element'

cards :: _
cards = do
  let go 0 _ acc = acc
      go n item acc = go (n - 1) (Set.nextCard item) (snoc acc item)
  shuffle $ go 81 Set.firstCard []

mainClass :: React.ReactClass { }
mainClass = React.component "Main" $ \this -> do
  cards' <- cards
  let
    setStatus todos todo status = fromMaybe todos $ do
      i <- elemIndex todo todos
      modifyAt i (\(Todo a) -> Todo a { status = status }) todos
    styles = Props.style {
      display: "grid",
      gridTemplateColumns: "repeat(4, auto)",
      maxWidth: "800px"
      }
    render _ = do
      DOM.div [styles] $ map Set.renderCard cards'

    -- render { todo, todos } = React.createLeafElement todoListClass {
    --   todos,
    --   todo,
    --   onAdd: \todo' -> do
    --     log ("Adding a new todo! " <> show todo')
    --     React.modifyState this \a -> a {
    --       todo = Nothing,
    --       todos = snoc a.todos todo'
    --       },
    --   onEdit: \todo' -> React.modifyState this _ {
    --     todo = Just todo'
    --     },
    --   onDone: \todo' -> React.modifyState this \a ->
    --     a { todos = setStatus a.todos todo' TodoDone },
    --   onClear : \todo' -> React.modifyState this \a ->
    --       a { todos = setStatus a.todos todo' TodoCleared }
    --   }

  pure {
    state: { todo: Nothing, todos: [] },
    render: render <$> React.getState this
    }
