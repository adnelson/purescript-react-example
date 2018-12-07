module Main where

import Common

import Data.Maybe (fromJust)
import Web.DOM.NonElementParentNode (getElementById) as DOM
import Web.HTML (window) as DOM
import Web.HTML.HTMLDocument (toNonElementParentNode) as DOM
import Web.HTML.Window (document) as DOM
import Partial.Unsafe (unsafePartial)

import React as React
import ReactDOM as ReactDOM
import Game.Set.Components.Game as Game

-- TODO start using for CSS
-- import CSS hiding (map)

main :: Effect Unit
main = void $ do
  document <- DOM.document =<< DOM.window
  let node = DOM.toNonElementParentNode document
  element <- DOM.getElementById "app" node
  let element' = unsafePartial (fromJust element)
  ReactDOM.render (React.createLeafElement Game.setGame { }) element'
