module Game.Set.Components.Game where

import Common
import Common.DOM (divClass) as DOM
import Data.HashSet as HS
import Data.Array (take, drop, length, snoc, filter)

import React as React
import React.DOM (button, main, text) as DOM
import React.DOM.Props as Props

import Game.Set.Card as Card
import Game.Set.Components.CardGrid (cardGrid)

setGame :: React.ReactClass { }
setGame = React.component "SetGame" $ \this -> do
  initialCards <- Card.newShuffledCards
  let
    selectCard card = React.modifyState this $ \s -> do
      let sc = case HS.member card s.selectedCards of
            true -> HS.delete card s.selectedCards
            false -> HS.insert card s.selectedCards
      case HS.toArray sc of
        cardSet@[card1, card2, card3]
          | Card.isSet card1 card2 card3 -> do
            let cards' = filter (\c -> not (HS.member c sc)) s.displayedCards
                cardsNeeded = if length cards' < 12 then 3 else 0
            s {
              sets = s.sets `snoc` [card1, card2, card3],
              selectedCards = foldr HS.delete sc cardSet,
              displayedCards = cards' <> (take cardsNeeded s.remainingCards),
              remainingCards = drop cardsNeeded s.remainingCards
              }
        _ -> s {
             selectedCards = sc
          }

    moreCards = React.modifyState this $ \s -> do
      s { displayedCards = s.displayedCards <> take 3 s.remainingCards,
          remainingCards = drop 3 s.remainingCards }

    render { displayedCards, selectedCards, remainingCards, sets } = do
      let status = case HS.toArray selectedCards of
            [card1, card2, card3] -> DOM.text "Not a set"
            _ -> DOM.text "Please select three cards."
      DOM.main [Props.className "Game"] [
        React.createLeafElement cardGrid {
          displayedCards,
          selectedCards,
          selectCard
          },
        DOM.divClass "Dashboard" [] [
          status,
          DOM.divClass "Sets" [] [
            DOM.text (show (length sets) <> " sets found.")
            ],
          DOM.button [
            Props.onClick $ \_ -> moreCards
            ] [
            DOM.text "More cards"
            ],
          DOM.divClass "remaining" [] [
            DOM.text (show (length remainingCards) <> " remaining")
            ]
          ]
        ]
  pure {
    state: {
       displayedCards: take 12 initialCards,
       remainingCards: drop 12 initialCards,
       selectedCards: HS.empty,
       sets: []
       },
    render: render <$> React.getState this
    }
