module Game.Set.Message where

import Common
import Common.DOM as DOM
import React.DOM (text) as DOM

import React (ReactElement)

data Message = NoMessage | BadMessage String | GoodMessage String

renderMessage :: Message -> ReactElement
renderMessage = case _ of
  NoMessage -> mempty
  BadMessage m -> DOM.divClass "Message bad" [] [DOM.text m]
  GoodMessage m -> DOM.divClass "Message" [] [DOM.text m]
