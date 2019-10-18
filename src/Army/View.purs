module Army.View where

import Prelude
import Army.Models (Side(..))
import Model (Region)
import React (ReactClass, statelessComponent)
import React.DOM.Props (height, href, key, width, x, y)
import Utils (image)

armyClass :: ReactClass { region :: Region, index :: Int, side :: Side }
armyClass =
  statelessComponent \{ region, index, side } ->
    image
      [ key $ show index <> "-army"
      , x region.position.x
      , y region.position.y
      , width "50"
      , height "33"
      , href case side of
          Union -> "assets/union_flag_small.png"
          Confederate -> "assets/confederate_flag_small.png"
      ]
      []
