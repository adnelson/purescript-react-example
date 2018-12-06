module Game.CardGrid where

import Prelude
import React as React
import Effect (Effect)
import Effect.Console (log)
import Data.HashSet as HS

import Data.Array as Array
import React as React
import React.DOM as DOM
import React.DOM.Props as Props
import Game.Set (Card, renderCard)

type Props = {
  cards :: Array Card,
  selectedCards :: HS.HashSet Card,
  selectCard :: Card -> Effect Unit
  }

cardGrid :: React.ReactClass Props
cardGrid = React.component "CardGrid" $ \this -> do
  let
    render { cards, selectedCards, selectCard } = do
      let
        numCols = Array.length cards `div` 3
        styles = Props.style {
          display: "grid",
          gridTemplateColumns: "repeat(" <> show numCols <> ", auto)",
          maxWidth: "800px"
          }
        mkCard card = do
          DOM.div [
            Props.onClick $ \_ -> do
              log ("selecting " <> show card)
              selectCard card
            ] [
            renderCard { card, isSelected: HS.member card selectedCards }
            ]
      DOM.section [styles] $ map mkCard cards
  pure {
    state: {},
    render: do
      props <- React.getProps this
      pure $ render props
    }
