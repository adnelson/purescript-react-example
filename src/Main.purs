module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Effect.Random as Random

import Data.HashSet (HashSet)
import Data.HashSet as HashSet
import Data.Tuple (Tuple(..), snd)
import Data.Array (snoc, modifyAt, elemIndex, cons, length, sortBy, zip, fromFoldable, take, mapWithIndex)
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

import Effect.Exception (error, throwException)
-- TODO start using for CSS
-- import CSS hiding (map)
import Data.Maybe (Maybe(..))
import Data.NonEmpty (singleton)

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
  log "Hello??"
  ReactDOM.render (React.createLeafElement mainClass { }) element'

shuffleCards :: Effect (Array Set.Card)
shuffleCards = do
  let go 0 _ acc = acc
      go n item acc = go (n - 1) (Set.nextCard item) (snoc acc item)
  shuffle $ go 81 Set.firstCard []

cardGridClass :: React.ReactClass { cards :: Array Set.Card }
cardGridClass = React.component "CardGrid" $ \this -> do
  let
    render { cards } { selected } = do
      let
        numCols = length cards `div` 3
        styles = Props.style {
          display: "grid",
          gridTemplateColumns: "repeat(" <> show numCols <> ", auto)",
          maxWidth: "800px"
          }
        selectCard index = React.modifyState this $ \s -> do
          s { selected = case HashSet.member index s.selected of
                true -> HashSet.delete index s.selected
                false -> HashSet.insert index s.selected }
        doCard index card = do
          DOM.div [
            Props.onClick $ \_ -> do
              log ("selecting " <> show index)
              selectCard index
            ] [
            Set.renderCard { card, isSelected: HashSet.member index selected }
            ]
      DOM.section [styles] $ mapWithIndex doCard cards
  pure {
    state: { selected: (HashSet.empty :: HashSet Int) },
    render: do
      props <- React.getProps this
      state <- React.getState this
      pure $ render props state
    }

mainClass :: React.ReactClass { }
mainClass = React.component "Main" $ \this -> do
  cards <- take 12 <$> shuffleCards
  let
    setStatus todos todo status = fromMaybe todos $ do
      i <- elemIndex todo todos
      modifyAt i (\(Todo a) -> Todo a { status = status }) todos

    header = DOM.header [] [DOM.text "Set"]
    render _ = DOM.main [] [
      header,
      React.createLeafElement cardGridClass { cards }
      ]
  pure {
    state: { todo: Nothing, todos: [] },
    render: render <$> React.getState this
    }
