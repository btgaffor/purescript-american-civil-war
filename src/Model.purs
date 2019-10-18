module Model where

import Prelude

import Army.Models (Army, Side)
import Control.Monad.Cont (ContT)
import Data.Maybe (Maybe)
import Data.Set (Set)
import Effect (Effect)
import React (ReactElement)
import React.DOM.Props (Props)

type Page a
  = ContT Unit Effect a

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
