module Game.Set where

import Prelude

import Data.Array (all, length, nub, replicate)
import Data.Generic.Rep (class Generic) as G
import Data.Generic.Rep.Ord as GO
import Data.Generic.Rep.Eq as GE
import Data.Generic.Rep.Show as GS

import React (ReactElement)
import React.DOM as DOM
import React.DOM.Props (style)
import React.DOM.Props as DOM
import React.DOM.SVG.Dynamic as SVG

--------------------------------------------------------------------------------
--- * Attribute: a generic "one of three choices" type.
data Attribute = First | Second | Third
derive instance genericAttribute :: G.Generic Attribute _

instance eqAttribute :: Eq Attribute where
  eq = GE.genericEq

instance ordAttribute :: Ord Attribute where
  compare = GO.genericCompare

instance showAttribute :: Show Attribute where
  show = GS.genericShow

type Card = {
  shape :: Attribute,
  color :: Attribute,
  shading :: Attribute,
  count :: Attribute
  }

firstCard :: Card
firstCard = {
  shape: First,
  color: First,
  shading: First,
  count: First
  }

nextCard :: Card -> Card
nextCard c = do
  let -- [shape, color, shading, count] = [c.shape, c.color, c.shading, c.count]
    inc First = {next: Second, carry: false}
    inc Second = {next: Third, carry: false}
    inc Third = {next: First, carry: true}

  case inc c.shape of
    { next: shape , carry: false} -> c { shape = shape }
    { next: shape, carry: true } -> case inc c.color of
      {next: color, carry: false } -> do
        c { shape = shape, color = color }
      {next: color, carry: true } -> case inc c.shading of
        {next: shading, carry: false } -> do
          c { shape = shape, color = color, shading = shading }
        {next: shading, carry: true } -> case inc c.count of
          {next: count, carry: false } -> do
            c { shape = shape, color = color, shading = shading, count = count }
          {next: count, carry: true } -> firstCard

cardColor :: Card -> String
cardColor { color } = case color of
  First -> "red"
  Second -> "green"
  Third -> "blue"

renderShape :: Card -> ReactElement
renderShape card@{ shape, shading } = elem (attrs <> sharedAttrs) [] where
  sharedAttrs = [stroke, DOM.strokeWidth 1, fill]
  stroke = DOM.stroke (cardColor card)
  fill = DOM.fill $ case shading of
    First -> "none"
    Second -> cardColor card
    Third -> "url(#verticalLines)"
  {elem, attrs} = case shape of
    First -> {
      elem: SVG.polygon,
      attrs: [
        DOM.points "0,25 50,0 100,25 50, 50",
        DOM.unsafeMkProps "rx" "5",
        DOM.unsafeMkProps "ry" "5"
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
             DOM._id "verticalLines",
             DOM.unsafeMkProps "patternUnits" "userSpaceOnUse",
             DOM.width "4",
             DOM.height "4"
             ] [
             SVG.path [
                DOM.unsafeMkProps "d" "M2,0 l0,4",
                DOM.stroke (cardColor card),
                DOM.unsafeMkProps "strokeWidth" "1.5"
                ] []
             ]
          ]
        _ -> []
      toSVG shape = flip SVG.svg (svgChildren <> [shape]) [
        style { width: "100px" },
        DOM.viewBox "-1 -1 102 52"
        ]
      styles = style {
        display: "flex",
        flexDirection: "column",
        width: "100px",
        height: "150px",
        padding: "15px",
        border: "1px solid black",
        borderRadius: "5px",
        marginBottom: "5px",
        justifyContent: "space-evenly"
        }
  DOM.div [DOM.className "Card", styles] $ map toSVG shapes

isSet :: Card -> Card -> Card -> Boolean
isSet a b c = all ok [shapes, colors, shadings, counts]
  where
    shapes = nub [a.shape, b.shape, c.shape]
    colors = nub [a.color, b.color, c.color]
    shadings = nub [a.shading, b.shading, c.shading]
    counts = nub [a.count, b.count, c.count]
    ok list = length list == 1 || length list == 3
