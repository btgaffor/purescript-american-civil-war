module Main where

import Prelude hiding (div)
import Army.Models (Side(..), otherSide)
import Board (RegionHighlight(..), boardClass, canMove, initialBoard, getDestinationIndex)
import Control.Monad.Cont (ContT(..), runContT)
import Data.Array (updateAt, (!!))
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Model (Model, Region, Page)
import React (ReactClass, createLeafElement, statelessComponent)
import React.DOM (button, div, h1, text)
import React.DOM.Props as Props
import Utils (mountApp)

main :: Effect Unit
main = do
  flip runContT identity do
    winner <- startGame initialModel
    pure $ gameOver winner

-----------------
-- game states --
-----------------
startGame :: Model -> Page Side
startGame model = case getWinner model of
  Just side -> pure side
  Nothing -> do
    newModel <- startTurn model
    startGame newModel

startTurn :: Model -> Page Model
startTurn model = do
  mainAction <- getMainAction model
  case mainAction of
    RegionClick fromIndex ->
      if (isCurrentPlayerArmy model.currentTurn model.map fromIndex) then do
        updatedModel <- startMovingArmy model fromIndex
        startTurn updatedModel
      else
        startTurn model
    EndTurn -> pure $ model { currentTurn = otherSide model.currentTurn }

startMovingArmy :: Model -> Int -> Page Model
startMovingArmy model fromIndex = do
  toIndex <- getDestinationIndex model fromIndex
  if toIndex == fromIndex then
    startTurn model
  else
    if canMove model.map fromIndex toIndex then
      startTurn $ doArmyMove fromIndex toIndex model
    else
      startMovingArmy model fromIndex

---------------
-- utilities --
---------------
getWinner :: Model -> Maybe Side
getWinner model = case model.currentTurn of
  Union -> Nothing
  Confederate -> Nothing

isCurrentPlayerArmy :: Side -> Array Region -> Int -> Boolean
isCurrentPlayerArmy currentTurn map index =
  fromMaybe false do
    army <- map !! index >>= _.army
    pure $ army.side == currentTurn

doArmyMove :: Int -> Int -> Model -> Model
doArmyMove fromIndex toIndex oldModel =
  fromMaybe oldModel do
    fromRegion <- oldModel.map !! fromIndex
    movingArmy <- fromRegion.army
    toRegion <- oldModel.map !! toIndex
    let
      updatedArmy = movingArmy { units = movingArmy.units # map \unit -> unit { moves = unit.moves - 1 } }
    updatedMap <-
      oldModel.map
        # updateAt fromIndex (fromRegion { army = Nothing })
        >>= updateAt toIndex (toRegion { army = Just updatedArmy })
    pure $ oldModel { map = updatedMap }

data MainAction
  = RegionClick Int
  | EndTurn

getMainAction :: Model -> Page MainAction
getMainAction model = ContT $ \next -> mountApp $ createLeafElement mainClass { model, next }

mainClass :: ReactClass { model :: Model, next :: MainAction -> Effect Unit }
mainClass =
  statelessComponent \{ model, next } ->
    div [ Props.className "flex-row" ]
      [ createLeafElement boardClass { model, regionHighlight: OwnedByPlayer, next: \index -> next $ RegionClick index }
      , button [ Props.className "end-turn", Props.onClick $ \_ -> next $ EndTurn ] [ text "End Turn" ]
      ]

gameOver :: Side -> Effect Unit
gameOver winner = mountApp $ createLeafElement gameOverClass { winner: winner }

gameOverClass :: ReactClass { winner :: Side }
gameOverClass =
  statelessComponent \{ winner } ->
    h1 [] [ text $ show winner ]

initialModel :: Model
initialModel =
  { currentTurn: Union
  , map: initialBoard
  }
