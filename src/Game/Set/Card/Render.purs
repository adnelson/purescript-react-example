module Game.Set.Card.Render where

import Common
import Common.DOM as DOM

import Data.Array as Array
import Data.HashSet as HS

import React (ReactElement)
import React.DOM as DOM
import React.DOM.Props as Props
import React.DOM.SVG.Dynamic as SVG

import Game.Set.Card.Types (Card, Attribute(..))
import Game.Set.Card.Utils (cardColor)

renderShape :: Card -> ReactElement
renderShape card@{ shape, shading } = elem (attrs <> sharedAttrs) [] where
  sharedAttrs = [stroke, Props.strokeWidth 2, fill]
  stroke = Props.stroke (cardColor card)
  fill = Props.fill $ case shading of
    First -> "none"
    Second -> cardColor card
    Third -> "url(#verticalLines" <> cardColor card <> ")"
  {elem, attrs} = case shape of
    First -> {
      elem: SVG.polygon,
      attrs: [
        Props.points "0,25 50,0 100,25 50, 50",
        Props.unsafeMkProps "rx" "15",
        Props.unsafeMkProps "ry" "15",
        Props.unsafeMkProps "strokeLinejoin" "round"
        ]
      }
    Second -> {
      elem: SVG.ellipse,
      attrs: [
        Props.unsafeMkProps "cx" "50",
        Props.unsafeMkProps "cy" "25",
        Props.unsafeMkProps "rx" "50",
        Props.unsafeMkProps "ry" "25"
        ]
      }
    Third -> {
      elem: SVG.rect,
      attrs: [
        Props.unsafeMkProps "x" "0",
        Props.unsafeMkProps "y" "0",
        Props.unsafeMkProps "width" "100",
        Props.unsafeMkProps "height" "50",
        Props.unsafeMkProps "rx" "5",
        Props.unsafeMkProps "ry" "5"
        ]
      }

renderCard :: Card -> ReactElement
renderCard card = do
  let shape = renderShape card
      shapes = case card.count of
        First -> [shape]
        Second -> [shape, shape]
        Third -> [shape, shape, shape]
      svgChildren = case card.shading of
        Third -> [
          SVG.pattern [
             Props._id ("verticalLines" <> cardColor card),
             Props.unsafeMkProps "patternUnits" "userSpaceOnUse",
             Props.width "4",
             Props.height "4"
             ] [
             SVG.rect [
                Props.unsafeMkProps "x" "0",
                Props.unsafeMkProps "y" "0",
                Props.unsafeMkProps "width" "4",
                Props.unsafeMkProps "height" "4",
                Props.fill "transparent"
                ] [],
             SVG.path [
                Props.unsafeMkProps "d" "M2,0 l0,4",
                Props.stroke (cardColor card),
                Props.unsafeMkProps "strokeWidth" "1.5"
                ] []
             ]
          ]
        _ -> []
      className = "Card"
      toSVG shape = flip SVG.svg (svgChildren <> [shape]) [
        Props.viewBox "-1 -1 102 52"
        ]
  DOM.divClass className [] $ map toSVG shapes

renderCardContainer :: {
  card :: Card,
  selectCard :: Card -> Effect Unit,
  isSelected :: Boolean
  } -> ReactElement
renderCardContainer { selectCard, card, isSelected } = do
  let className = "CardContainer" <> if isSelected then " selected" else ""
      props = [Props.onClick $ \_ -> selectCard card]
  DOM.divClass className props [renderCard card]

renderCardGrid :: forall r. {
  displayedCards :: Array Card,
  selectedCards :: HS.HashSet Card,
  selectCard :: Card -> Effect Unit
  | r } -> ReactElement
renderCardGrid { displayedCards, selectedCards, selectCard } = do
  let
    numCols = Array.length displayedCards `div` 3
    mkCard card = do
      let isSelected = HS.member card selectedCards
      renderCardContainer { card, selectCard, isSelected }
  DOM.divClass "CardGrid space-fill" [] $
    map mkCard displayedCards
