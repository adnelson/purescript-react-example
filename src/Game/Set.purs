module Game.Set (Attribute(..), renderCard) where

import Prelude

import Undefined
import Data.Array (all, length, nub)
import Data.Generic.Rep (class Generic) as G
import Data.Generic.Rep.Ord as GO
import Data.Generic.Rep.Eq as GE
import Data.Generic.Rep.Show as GS

import React (ReactElement)
import React.DOM as DOM
import React.DOM.Props as DOMProps
import React.DOM.SVG.Dynamic as SVG

-- import Spork.Html (Html)
-- import Spork.Html as H

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

renderShape :: Card -> ReactElement
renderShape { shape, shading } = elem (attrs <> [fill, stroke]) [] where
  attr = DOMProps.unsafeMkProps
  stroke = attr "stroke" "currentColor"
  fill = attr "fill" $ case shading of
    First -> "none"
    Second -> "currentColor"
    Third -> "url(#verticalLines)"
  {elem, attrs} = case shape of
    First -> {
      elem: SVG.polygon,
      attrs: [attr "points" "0,25 50,0 100,25 50, 50"]
      }
    Second -> {
      elem: SVG.ellipse,
      attrs: [
        attr "cx" "50",
        attr "cy" "25",
        attr "rx" "50",
        attr "ry" "25"
        ]
      }
    Third -> {
      elem: SVG.rect,
      attrs: [
        attr "x" "0",
        attr "y" "0",
        attr "width" "100",
        attr "height" "50"
        ]
      }

renderCard :: Card -> ReactElement
renderCard card = SVG.svg [] [
  renderShape card
  ]

isSet :: Card -> Card -> Card -> Boolean
isSet a b c = all ok [shapes, colors, shadings, counts]
  where
    shapes = nub [a.shape, b.shape, c.shape]
    colors = nub [a.color, b.color, c.color]
    shadings = nub [a.shading, b.shading, c.shading]
    counts = nub [a.count, b.count, c.count]
    ok list = length list == 1 || length list == 3
