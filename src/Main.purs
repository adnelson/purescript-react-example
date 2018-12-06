module Main where

import Prelude

import Effect (Effect)
import Effect.Random as Random

import Data.HashSet as HS
import Data.Tuple (Tuple(..), snd)
import Data.Array (snoc, length, sortBy, zip, fromFoldable, take)
import Data.List.Lazy (replicateM)
import Data.Maybe (Maybe(..), fromJust)

import Web.HTML.HTMLDocument (toNonElementParentNode) as DOM
import Web.DOM.NonElementParentNode (getElementById) as DOM
import Web.HTML (window) as DOM
import Web.HTML.Window (document) as DOM
import React.DOM (header, main, section, text) as DOM
import React.DOM.Props as Props

import Partial.Unsafe (unsafePartial)

import React as React
import ReactDOM as ReactDOM

import Game.Set as Set
import Game.CardGrid (cardGrid)

-- TODO start using for CSS
-- import CSS hiding (map)

shuffle :: forall a. Array a -> Effect (Array a)
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

shuffleCards :: Effect (Array Set.Card)
shuffleCards = do
  let go 0 _ acc = acc
      go n item acc = go (n - 1) (Set.nextCard item) (snoc acc item)
  shuffle $ go 81 Set.firstCard []

mainClass :: React.ReactClass { }
mainClass = React.component "Main" $ \this -> do
  initialCards <- take 12 <$> shuffleCards
  let
    header = DOM.header [] [DOM.text "Set"]
    selectCard index = React.modifyState this $ \s -> do
      s { selectedCards = case HS.member index s.selectedCards of
            true -> HS.delete index s.selectedCards
            false -> HS.insert index s.selectedCards }
    render { cards, selectedCards } = do
      let status = case HS.toArray selectedCards of
            [card1, card2, card3]
              | Set.isSet card1 card2 card3 -> DOM.text "Set!"
              | otherwise -> DOM.text "Not a set"
            _ -> DOM.text "Please select three cards."
      DOM.main [] [
        header,
        React.createLeafElement cardGrid {
          cards: take 12 cards,
          selectedCards,
          selectCard
          },
        DOM.section [Props.className "info"] [
          status
          ]
        ]
  pure {
    state: {
       cards: initialCards,
       selectedCards: HS.empty
       },
    render: render <$> React.getState this
    }
