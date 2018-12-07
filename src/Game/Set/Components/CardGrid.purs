module Game.Set.Components.CardGrid where

import Common
import Common.DOM (divClass) as DOM

import Effect.Console (log)
import Data.HashSet as HS

import Data.Array as Array
import React as React
import React.DOM.Props as Props
import Game.Set.Card (Card, renderCard)

type Props = {
  displayedCards :: Array Card,
  selectedCards :: HS.HashSet Card,

  selectCard :: Card -> Effect Unit
  }

cardGrid :: React.ReactClass Props
cardGrid = React.component "CardGrid" $ \this -> do
  let
    render { displayedCards, selectedCards, selectCard } = do
      let
        numCols = Array.length displayedCards `div` 3
        mkCard card = do
          DOM.divClass "CardContainer" [
            Props.onClick $ \_ -> do
              log ("selecting " <> show card)
              selectCard card
            ] [
            renderCard { card, isSelected: HS.member card selectedCards }
            ]
      DOM.divClass "CardGrid space-fill" [] $
        map mkCard displayedCards
  pure {
    state: {},
    render: do
      props <- React.getProps this
      pure $ render props
    }
