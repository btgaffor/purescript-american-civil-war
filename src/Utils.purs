module Utils where

import Prelude
import Data.Array (catMaybes, concat, mapWithIndex)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log, error)
import React as React
import React.DOM (IsDynamic(..), mkDOM)
import React.DOM.Props as Props
import ReactDOM as ReactDOM
import Web.DOM.NonElementParentNode (getElementById) as DOM
import Web.HTML (window) as DOM
import Web.HTML.HTMLDocument (toNonElementParentNode) as DOM
import Web.HTML.Window (document) as DOM

mountApp :: React.ReactElement -> Effect Unit
mountApp app =
  void
    $ DOM.window
    >>= DOM.document
    >>= (DOM.toNonElementParentNode >>> pure)
    >>= DOM.getElementById "example"
    >>= \element -> case element of
        Nothing -> error "Could not find DOM node to mount on" *> pure Nothing
        Just node -> ReactDOM.render app node

image :: Array Props.Props -> Array React.ReactElement -> React.ReactElement
image = mkDOM (IsDynamic false) "image"

concatMapWithIndex :: forall a b. (Int -> a -> Array b) -> Array a -> Array b
concatMapWithIndex f a = concat $ mapWithIndex f a
