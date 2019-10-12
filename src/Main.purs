module Main where

import Prelude hiding (div)
import Control.Monad.Cont (ContT(..), runContT)
import Data.Array (catMaybes, concat, cons, index, mapWithIndex, updateAt, (!!))
import Data.List (List(..), singleton)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Effect (Effect)
import Effect.Console (log)
import React (ReactClass, ReactElement, createLeafElement, statelessComponent)
import React.DOM (button, div, h1, img, int, text)
import React.DOM.Props as Props
import React.DOM.SVG (svg, polygon, circle)
import React.SyntheticEvent (SyntheticMouseEvent)
import Unsafe.Coerce (unsafeCoerce)
import Utils (concatMapWithIndex, image, mountApp, toMaybe)
import Data.Set as Set

type Page a
  = ContT Unit Effect a

main :: Effect Unit
main = do
  flip runContT identity
    $ do
        winner <- playGame initialModel
        pure $ mountApp $ createLeafElement gameOverClass { winner: winner, next: main }

playGame :: Model -> Page Side
playGame model = case getWinner model of
  Just side -> pure side
  Nothing -> do
    newModel <- playTurn model
    playGame newModel

getWinner :: Model -> Maybe Side
getWinner model = case model.currentTurn of
  Union -> Nothing
  Confederate -> Nothing

playTurn :: Model -> Page Model
playTurn model = do
  mainAction <- ContT $ \next -> mountApp $ createLeafElement mainClass { model, next }
  case mainAction of
    RegionClick fromIndex ->
      if (isCurrentPlayerArmy model.currentTurn model.map fromIndex) then do
        toIndex <- ContT $ \next -> mountApp $ createLeafElement board { model, next, regionHighlight: DestinationsFrom fromIndex }
        pure $ doArmyMove fromIndex toIndex model
      else
        playTurn model
    EndTurn ->
      pure
        $ model
            { currentTurn =
              case model.currentTurn of
                Union -> Confederate
                Confederate -> Union
            }

test :: Boolean
test = isCurrentPlayerArmy Union initialModel.map 0

isCurrentPlayerArmy :: Side -> Array Region -> Int -> Boolean
isCurrentPlayerArmy currentTurn map index =
  fromMaybe false do
    army <- map !! index >>= _.army
    pure $ army.side == currentTurn

doArmyMove :: Int -> Int -> Model -> Model
doArmyMove fromIndex toIndex oldModel =
  fromMaybe oldModel do
    fromRegion <- oldModel.map !! fromIndex
    movingArmy <- pure fromRegion.army
    toRegion <- oldModel.map !! toIndex
    updatedMap <-
      oldModel.map
        # updateAt fromIndex (fromRegion { army = Nothing })
        >>= updateAt toIndex (toRegion { army = movingArmy })
    pure $ oldModel { map = updatedMap }

data MainAction
  = RegionClick Int
  | EndTurn

mainClass :: ReactClass { model :: Model, next :: MainAction -> Effect Unit }
mainClass =
  statelessComponent \{ model, next } ->
    div [ Props.className "flex-row" ]
      [ createLeafElement board { model, regionHighlight: OwnedByPlayer, next: \index -> next $ RegionClick index }
      , button [ Props.className "end-turn", Props.onClick $ \_ -> next $ EndTurn ] [ text "End Turn" ]
      ]

gameOverClass :: ReactClass { winner :: Side, next :: Effect Unit }
gameOverClass =
  statelessComponent \{ winner } ->
    h1 [] [ text $ show winner ]

data RegionHighlight
  = OwnedByPlayer
  | DestinationsFrom Int

board :: ReactClass { model :: Model, regionHighlight :: RegionHighlight, next :: (Int -> Effect Unit) }
board =
  statelessComponent \{ model, regionHighlight, next } ->
    div [ Props.className "flex-column" ]
      [ createLeafElement mapHeader { header: "Turn: " <> show model.currentTurn }
      , svg
          [ Props.className "map", Props.width "658px", Props.height "415px" ]
          $ cons
              (image [ Props.href "assets/map_v2.png", Props.width "658", Props.height "415" ] [])
              ( concatMapWithIndex
                  ( \index region ->
                      catMaybes
                        [ region.army # map \army -> createLeafElement armyClass { region, index, side: army.side }
                        , Just
                            $ region.shape
                                ( catMaybes
                                    [ case regionHighlight of
                                        OwnedByPlayer -> (region.army <#> _.side >>> (==) model.currentTurn) >>= toMaybe (Props.className "highlight green")
                                        DestinationsFrom fromIndex ->
                                          if index == fromIndex then
                                            Just (Props.className "permenant yellow")
                                          else
                                            (model.map !! fromIndex <#> _.connections >>> Set.member index) >>= toMaybe (Props.className "permenant green")
                                    , Just $ Props.key $ show index <> "-region"
                                    , Just $ Props.onClick \e -> (logRegionClick e index) *> next index
                                    ]
                                )
                        ]
                  )
                  model.map
              )
      ]

mapHeader :: ReactClass { header :: String }
mapHeader = statelessComponent \{ header } -> h1 [] [ text header ]

logRegionClick :: SyntheticMouseEvent -> Int -> Effect Unit
logRegionClick event index = log $ "Clicked region " <> show index <> " at " <> (unsafeCoerce event).pageX <> "," <> (unsafeCoerce event).pageY

armyClass :: ReactClass { region :: Region, index :: Int, side :: Side }
armyClass =
  statelessComponent \{ region, index, side } ->
    image
      [ Props.key $ show index <> "-army"
      , Props.x region.position.x
      , Props.y region.position.y
      , Props.width "50"
      , Props.height "33"
      , Props.href case side of
          Union -> "assets/union_flag_small.png"
          Confederate -> "assets/confederate_flag_small.png"
      ]
      []

data Side
  = Union
  | Confederate

instance showSide :: Show Side where
  show Union = "Union"
  show Confederate = "Confederate"

derive instance eqSide :: Eq Side

data UnitType
  = Infantry
  | Cavalry
  | EliteCavalry
  | Artillary
  | Leader

type Army
  = { side :: Side
    , units :: List ArmyUnit
    }

type ArmyUnit
  = { unitType :: UnitType, moves :: Int }

type Region
  = { army :: Maybe Army
    , shape :: Array Props.Props -> ReactElement
    , position :: { x :: Int, y :: Int }
    , connections :: Set.Set Int
    }

type Model
  = { currentTurn :: Side
    , map :: Array Region
    }

initialModel :: Model
initialModel =
  { currentTurn: Union
  , map:
    -- 0
    [ { army: Just { side: Union, units: Nil }
      , shape: \shapeProps -> polygon (concat [ shapeProps, [ Props.points "8,9 269,9 202,129 134,179 86,183 35,195 6,264" ] ]) []
      , position: { x: 80, y: 70 }
      , connections: Set.fromFoldable [ 1, 2, 12 ]
      }
    -- 1
    , { army: Just { side: Union, units: Nil }
      , shape: \shapeProps -> polygon (concat [ shapeProps, [ Props.points "268,10 346,9 348,211 234,210 202,131" ] ]) []
      , position: { x: 255, y: 105 }
      , connections: Set.fromFoldable [ 0, 2, 3, 4, 6 ]
      }
    -- 2
    , { army: Just { side: Union, units: Nil }
      , shape: \shapeProps -> polygon (concat [ shapeProps, [ Props.points "5,268 39,197 88,183 133,179 202,132 232,211 255,270" ] ]) []
      , position: { x: 100, y: 215 }
      , connections: Set.fromFoldable [ 0, 1, 3, 11, 12 ]
      }
    -- 3
    , { army: Just { side: Union, units: Nil }
      , shape: \shapeProps -> polygon (concat [ shapeProps, [ Props.points "233,211 348,210 347,270 257,270" ] ]) []
      , position: { x: 270, y: 220 }
      , connections: Set.fromFoldable [ 1, 2, 9, 10 ]
      }
    -- 4
    , { army: Just { side: Union, units: Nil }
      , shape: \shapeProps -> polygon (concat [ shapeProps, [ Props.points "349,9 527,6 524,88 512,134 347,135" ] ]) []
      , position: { x: 360, y: 15 }
      , connections: Set.fromFoldable [ 1, 5, 6, 13 ]
      }
    -- 5
    , { army: Just { side: Union, units: singleton { unitType: Infantry, moves: 1 } }
      , shape: \shapeProps -> polygon (concat [ shapeProps, [ Props.points "529,8 652,9 653,133 512,132 526,86" ] ]) []
      , position: { x: 563, y: 53 }
      , connections: Set.fromFoldable [ 4, 7 ]
      }
    -- 6
    , { army: Just { side: Confederate, units: Nil }
      , shape: \shapeProps -> polygon (concat [ shapeProps, [ Props.points "349,134 514,135 494,166 462,201 432,216 380,215 350,210" ] ]) []
      , position: { x: 385, y: 153 }
      , connections: Set.fromFoldable [ 1, 4, 7, 9 ]
      }
    -- 7
    , { army: Just { side: Confederate, units: Nil }
      , shape: \shapeProps -> polygon (concat [ shapeProps, [ Props.points "514,134 651,134 653,256 632,259 563,307 523,312 486,282 483,242 464,203" ] ]) []
      , position: { x: 537, y: 193 }
      , connections: Set.fromFoldable [ 5, 6, 8, 9, 14 ]
      }
    -- 8
    , { army: Just { side: Confederate, units: Nil }
      , shape: \shapeProps -> polygon (concat [ shapeProps, [ Props.points "522,314 561,309 633,259 653,257 651,404 471,405 483,330" ] ]) []
      , position: { x: 545, y: 340 }
      , connections: Set.fromFoldable [ 7, 9, 14 ]
      }
    -- 9
    , { army: Just { side: Confederate, units: Nil }
      , shape: \shapeProps -> polygon (concat [ shapeProps, [ Props.points "347,211 388,215 435,215 461,205 482,244 485,280 482,331 472,406 349,406" ] ]) []
      , position: { x: 383, y: 290 }
      , connections: Set.fromFoldable [ 6, 7, 8, 14, 10, 3 ]
      }
    -- 10
    , { army: Just { side: Confederate, units: Nil }
      , shape: \shapeProps -> polygon (concat [ shapeProps, [ Props.points "258,270 346,271 349,408 259,408 249,385 264,300" ] ]) []
      , position: { x: 278, y: 325 }
      , connections: Set.fromFoldable [ 3, 9, 11 ]
      }
    -- 11
    , { army: Just { side: Confederate, units: Nil }
      , shape: \shapeProps -> polygon (concat [ shapeProps, [ Props.points "7,271 253,271 264,301 248,387 258,409 7,409" ] ]) []
      , position: { x: 30, y: 295 }
      , connections: Set.fromFoldable [ 2, 10, 15 ]
      }
    -- 12
    , { army: Just { side: Union, units: Nil }
      , shape: \shapeProps -> circle (concat [ shapeProps, [ Props.cx 107, Props.cy 180, Props.r 23 ] ]) []
      , position: { x: 83, y: 163 }
      , connections: Set.fromFoldable [ 0, 2 ]
      }
    -- 13
    , { army: Just { side: Union, units: Nil }
      , shape: \shapeProps -> circle (concat [ shapeProps, [ Props.cx 436, Props.cy 70, Props.r 24 ] ]) []
      , position: { x: 412, y: 53 }
      , connections: Set.fromFoldable [ 4 ]
      }
    -- 14
    , { army: Just { side: Confederate, units: Nil }
      , shape: \shapeProps -> circle (concat [ shapeProps, [ Props.cx 492, Props.cy 305, Props.r 24 ] ]) []
      , position: { x: 468, y: 287 }
      , connections: Set.fromFoldable [ 7, 8, 9 ]
      }
    -- 15
    , { army: Just { side: Confederate, units: Nil }
      , shape: \shapeProps -> circle (concat [ shapeProps, [ Props.cx 136, Props.cy 345, Props.r 21 ] ]) []
      , position: { x: 111, y: 328 }
      , connections: Set.fromFoldable [ 11 ]
      }
    ]
  }
