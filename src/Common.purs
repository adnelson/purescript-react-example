module Common (
  module Prelude,
  module Effect,
  module Data.Foldable,
  shuffle
  ) where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Foldable (class Foldable, foldr)
import Effect (Effect)
import Data.Array (length, fromFoldable, sortBy, zip)
import Data.List.Lazy (replicateM)
import Data.Tuple (Tuple(..), snd)
import Effect (Effect)
import Effect.Random as Random

-- | TODO implement in JavaScript?
shuffle :: forall a. Array a -> Effect (Array a)
shuffle xs = do
  let len = length xs
  randoms <- fromFoldable <$> replicateM len (Random.randomInt 0 (len - 1))
  pure $ map snd $ sortBy compareFst $ zip randoms xs
  where compareFst (Tuple a _) (Tuple b _) = compare a b
