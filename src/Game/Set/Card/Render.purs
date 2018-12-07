module Game.Set.Card.Render where

import Prelude

import React (ReactElement)
import React.DOM as DOM
import React.DOM.Props as DOM
import React.DOM.SVG.Dynamic as SVG

import Game.Set.Card.Types (Card, Attribute(..))
import Game.Set.Card.Utils (cardColor)

renderShape :: Card -> ReactElement
renderShape card@{ shape, shading } = elem (attrs <> sharedAttrs) [] where
  sharedAttrs = [stroke, DOM.strokeWidth 2, fill]
  stroke = DOM.stroke (cardColor card)
  fill = DOM.fill $ case shading of
    First -> "none"
    Second -> cardColor card
    Third -> "url(#verticalLines" <> cardColor card <> ")"
  {elem, attrs} = case shape of
    First -> {
      elem: SVG.polygon,
      attrs: [
        DOM.points "0,25 50,0 100,25 50, 50",
        DOM.unsafeMkProps "rx" "15",
        DOM.unsafeMkProps "ry" "15",
        DOM.unsafeMkProps "strokeLinejoin" "round"
        ]
      }
    Second -> {
      elem: SVG.ellipse,
      attrs: [
        DOM.unsafeMkProps "cx" "50",
        DOM.unsafeMkProps "cy" "25",
        DOM.unsafeMkProps "rx" "50",
        DOM.unsafeMkProps "ry" "25"
        ]
      }
    Third -> {
      elem: SVG.rect,
      attrs: [
        DOM.unsafeMkProps "x" "0",
        DOM.unsafeMkProps "y" "0",
        DOM.unsafeMkProps "width" "100",
        DOM.unsafeMkProps "height" "50",
        DOM.unsafeMkProps "rx" "5",
        DOM.unsafeMkProps "ry" "5"
        ]
      }

renderCard :: { card :: Card, isSelected :: Boolean } -> ReactElement
renderCard { card, isSelected } = do
  let shape = renderShape card
      shapes = case card.count of
        First -> [shape]
        Second -> [shape, shape]
        Third -> [shape, shape, shape]
      svgChildren = case card.shading of
        Third -> [
          SVG.pattern [
             DOM._id ("verticalLines" <> cardColor card),
             DOM.unsafeMkProps "patternUnits" "userSpaceOnUse",
             DOM.width "4",
             DOM.height "4"
             ] [
             SVG.rect [
                DOM.unsafeMkProps "x" "0",
                DOM.unsafeMkProps "y" "0",
                DOM.unsafeMkProps "width" "4",
                DOM.unsafeMkProps "height" "4",
                DOM.fill "transparent"
                ] [],
             SVG.path [
                DOM.unsafeMkProps "d" "M2,0 l0,4",
                DOM.stroke (cardColor card),
                DOM.unsafeMkProps "strokeWidth" "1.5"
                ] []
             ]
          ]
        _ -> []
      className = DOM.className ("Card" <> if isSelected then " selected" else "")
      toSVG shape = flip SVG.svg (svgChildren <> [shape]) [
        DOM.viewBox "-1 -1 102 52"
        ]
  DOM.div [className] $ map toSVG shapes
