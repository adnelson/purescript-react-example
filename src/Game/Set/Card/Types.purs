module Game.Set.Card.Types where

import Prelude

import Data.Hashable (class Hashable)
import Data.Array (all, length, nub, replicate)
import Data.Generic.Rep (class Generic) as G
import Data.Generic.Rep.Ord as GO
import Data.Generic.Rep.Eq as GE
import Data.Generic.Rep.Show as GS

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

instance hashableAttribute :: Hashable Attribute where
  hash attr = case attr of
    First -> 1
    Second -> 2
    Third -> 3

type Card = {
  shape :: Attribute,
  color :: Attribute,
  shading :: Attribute,
  count :: Attribute
  }
