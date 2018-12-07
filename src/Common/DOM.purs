module Common.DOM (
  divClass
  ) where

import Common
import React (ReactElement)
import React.DOM (div) as DOM
import React.DOM.Props (Props, className) as Props

divClass :: String -> Array Props.Props -> Array ReactElement -> ReactElement
divClass name props = DOM.div ([Props.className name] <> props)
