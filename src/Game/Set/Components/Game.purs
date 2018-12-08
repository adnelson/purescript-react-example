module Game.Set.Components.Game where

import Common
import Common.DOM (divClass) as DOM
import Effect.Timer as Timer
import Data.UUID (UUID)
import Data.HashSet as HS
import Data.Array (take, drop, length, snoc, filter)

import React as React
import React.DOM (button, main, text) as DOM
import React.DOM.Props as Props

import Game.Set.Card as Card
import Game.Set.Message (Message(..), renderMessage)
import Game.Set.Card (Card)
import Game.Set.Card.Utils (findSets)

data IsSet = NotThreeCards | YesSet (Array Card) | NotSet

type State = {
  displayedCards :: Array Card,
  remainingCards :: Array Card,
  selectedCards :: HS.HashSet Card,
  sets :: Array (Array Card),
  score :: Int,
  seconds :: Int,
  message :: Message,
  messages :: Array { message :: Message, id :: UUID }
  }

setGame :: React.ReactClass { }
setGame = React.component "SetGame" $ \this -> do
  initialCards <- Card.newShuffledCards
  let
    initialTime = 60
    countDown = React.modifyState this $ \s -> do
      let timedOut = s.seconds == 1
      s {
        seconds = if timedOut then initialTime else s.seconds - 1,
        score = if timedOut then s.score - 1 else s.score,
        message = if not timedOut then NoMessage else BadMessage "Out of time!"
        }

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
            score = s.score + 3,
            seconds = s.seconds + 60,
            message = GoodMessage "You found a set!"
            }
        NotSet -> s {
          score = s.score - 1,
          selectedCards = HS.empty :: HS.HashSet Card,
          message = BadMessage "That wasn't a set!"
          }

    moreCards = React.modifyState this $ \s -> do
      let numMissedSets = length (findSets s.displayedCards)
      s { displayedCards = s.displayedCards <> take 3 s.remainingCards,
          remainingCards = drop 3 s.remainingCards,
          score = if numMissedSets > 0 then s.score - 1 else s.score,
          message =
            if numMissedSets == 0 then NoMessage
            else BadMessage ("You missed " <> show numMissedSets <> " sets!")
        }

    render state = do
      let { displayedCards, selectedCards, remainingCards, sets, score,
            seconds, message } = state
      DOM.main [Props.className "SetGame space-fill"] [
        Card.renderCardGrid { displayedCards, selectedCards, selectCard },
        DOM.divClass "Dashboard" [] [
          DOM.divClass "Time" [] [
             DOM.text (show seconds <> " seconds before penalty")
             ],
          DOM.divClass "Score" [] [
             DOM.text ("Score: " <> show score)
             ],
          DOM.divClass "Sets" [] $ do
            let setElems = flip map sets $ \set -> do
                  DOM.divClass "Set" [] $ map Card.renderCard set
            setElems <> [DOM.text (show (length sets) <> " sets found.")],
          DOM.divClass "CardsRemaining" [] [
            DOM.text (show (length remainingCards) <> " cards remaining")
            ],
          DOM.button [
            Props.onClick $ \_ -> moreCards
            ] [
            DOM.text "Deal More Cards"
            ],
          renderMessage message
          ]
        ]
  pure {
    state: {
      displayedCards: take 12 initialCards,
      remainingCards: drop 12 initialCards,
      selectedCards: HS.empty,
      sets: [],
      score: 0,
      seconds: initialTime,
      message: NoMessage,
      messages: []
      },
    componentDidMount: void (Timer.setInterval 1000 countDown),
    componentDidUpdate: \_ { message } _ -> case message of
      NoMessage -> pure unit
      _ -> void $ Timer.setTimeout 3000 $ React.modifyState this $ \s ->
        s { message = NoMessage },
    render: render <$> React.getState this
    }
