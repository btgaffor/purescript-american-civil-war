module Model where

import Prelude
import Army.Models (Army, Side)
import Data.List (List)
import Data.Maybe (Maybe)
import Data.Set (Set)
import React (ReactElement)
import React.DOM.Props (Props)

type Model
  = { currentTurn :: Side
    , map :: Board
    }

type Region
  = { army :: Maybe Army
    , shape :: Array Props -> ReactElement
    , position :: { x :: Int, y :: Int }
    , connections :: Set Int
    }

type Board
  = Array Region
