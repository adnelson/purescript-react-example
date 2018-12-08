module Game.Set.Card.Utils (
  firstCard, nextCard, cardColor, newShuffledCards, findSets, isSet
  ) where

import Common

import Data.Function.Uncurried (Fn3, runFn3)
import Data.Array (all, snoc, length, nub, replicate)

import Game.Set.Card.Types (Card, Attribute(..))

firstCard :: Card
firstCard = {
  shape: First,
  color: First,
  shading: First,
  count: First
  }

nextCard :: Card -> Card
nextCard c = do
  let
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

allCards :: Array Card
allCards = go 81 firstCard [] where
  go 0 _ acc = acc
  go n item acc = go (n - 1) (nextCard item) (snoc acc item)

newShuffledCards :: Effect (Array Card)
newShuffledCards = shuffle allCards



foreign import isSet__ :: Fn3 Card Card Card Boolean


isSet :: Card -> Card -> Card -> Boolean
isSet = runFn3 isSet__

{-
isSet a b c = all ok [shapes, colors, shadings, counts]
  where
    shapes = nub [a.shape, b.shape, c.shape]
    colors = nub [a.color, b.color, c.color]
    shadings = nub [a.shading, b.shading, c.shading]
    counts = nub [a.count, b.count, c.count]
    ok list = length list == 1 || length list == 3
-}
foreign import findSets :: Array Card -> Array (Array Card)
