module Game.Set.Components.Game where

import Common
import Common.DOM (divClass) as DOM
import Data.HashSet as HS
import Data.Array (take, drop, length, snoc, filter)

import React as React
import React.DOM (button, main, text) as DOM
import React.DOM.Props as Props

import Game.Set.Card as Card
import Game.Set.Card (Card)
import Game.Set.Components.CardGrid (cardGrid)

data IsSet = NotThreeCards | YesSet (Array Card) | NotSet

setGame :: React.ReactClass { }
setGame = React.component "SetGame" $ \this -> do
  initialCards <- Card.newShuffledCards
  let
    checkSet sc = case HS.toArray sc of
      cardSet@[card1, card2, card3]
        | Card.isSet card1 card2 card3 -> YesSet [card1, card2, card3]
        | otherwise -> NotSet
      _ -> NotThreeCards

    selectCard card = React.modifyState this $ \s -> do
      let sc = case HS.member card s.selectedCards of
            true -> HS.delete card s.selectedCards
            false -> HS.insert card s.selectedCards
      case checkSet sc of
        NotThreeCards -> s { selectedCards = sc }
        YesSet set -> do
          let cards' = filter (\c -> not (HS.member c sc)) s.displayedCards
              cardsNeeded = if length cards' < 12 then 3 else 0
          s {
            sets = s.sets `snoc` set,
            selectedCards = HS.empty :: HS.HashSet Card,
            displayedCards = cards' <> (take cardsNeeded s.remainingCards),
            remainingCards = drop cardsNeeded s.remainingCards,
            score = s.score + 3
            }
        NotSet -> s {
          score = s.score - 1,
          selectedCards = HS.empty :: HS.HashSet Card
          }

    moreCards = React.modifyState this $ \s -> do
      s { displayedCards = s.displayedCards <> take 3 s.remainingCards,
          remainingCards = drop 3 s.remainingCards,
          score = s.score - 1 }

    render { displayedCards, selectedCards, remainingCards, sets, score } = do
      DOM.main [Props.className "SetGame space-fill"] [
        React.createLeafElement cardGrid {
          displayedCards,
          selectedCards,
          selectCard
          },
        DOM.divClass "Dashboard" [] [
          DOM.divClass "Score" [] [
             DOM.text ("Score: " <> show score)
             ],
          DOM.divClass "Sets" [] [
            DOM.text (show (length sets) <> " sets found.")
            ],
          DOM.button [
            Props.onClick $ \_ -> moreCards
            ] [
            DOM.text "More cards"
            ],
          DOM.divClass "remaining" [] [
            DOM.text (show (length remainingCards) <> " cards remaining")
            ]
          ]
        ]
  pure {
    state: {
       displayedCards: take 12 initialCards,
       remainingCards: drop 12 initialCards,
       selectedCards: HS.empty,
       sets: [],
       score: 0
       },
    render: render <$> React.getState this
    }
