module Board where

import Prelude hiding (div)

import Army.Models (Side(..), infantry, cavalry)
import Army.View (armyClass)
import Control.Monad.Cont (ContT(..))
import Control.MonadZero (guard)
import Data.Array (all, catMaybes, concat, cons, (!!))
import Data.Array.NonEmpty (singleton, toArray, (:))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set as Set
import Effect (Effect)
import Effect.Console (log)
import Model (Page, Board, Model, Region)
import React (ReactClass, createLeafElement, statelessComponent)
import React.DOM (div, h1, li, text, ul)
import React.DOM.Props (className, cx, cy, height, href, points, r, width, key, onClick)
import React.DOM.SVG (svg, polygon, circle)
import React.SyntheticEvent (SyntheticMouseEvent)
import Unsafe.Coerce (unsafeCoerce)
import Utils (concatMapWithIndex, image, mountApp)

-- Types
data RegionHighlight
  = OwnedByPlayer
  | DestinationsFrom Int

-- Utilities
logRegionClick :: SyntheticMouseEvent -> Int -> Effect Unit
logRegionClick event index = log $ "Clicked region " <> show index <> " at " <> (unsafeCoerce event).pageX <> "," <> (unsafeCoerce event).pageY

regionsConnected :: Array Region -> Int -> Int -> Boolean
regionsConnected map fromIndex toIndex = (map !! fromIndex <#> _.connections >>> Set.member toIndex) == Just true

canMove :: Array Region -> Int -> Int -> Boolean
canMove map fromIndex toIndex =
  fromMaybe false do
    fromRegion <- map !! fromIndex
    guard $ Set.member toIndex (fromRegion.connections)
    army <- fromRegion.army
    guard $ all (\unit -> unit.moves > 0) army.units
    pure $ true

-- View
mapHeader :: ReactClass { header :: String }
mapHeader = statelessComponent \{ header } -> h1 [] [ text header ]

getDestinationIndex :: Model -> Int -> Page Int
getDestinationIndex model fromIndex = ContT $ \next -> mountApp $ createLeafElement armyInfoClass { model, next, fromIndex }

armyInfoClass :: ReactClass { model :: Model, next :: Int -> Effect Unit, fromIndex :: Int }
armyInfoClass =
  statelessComponent \{ model, next, fromIndex } ->
    div [ className "flex-row" ]
      $ catMaybes
          [ Just $ createLeafElement boardClass { model, regionHighlight: DestinationsFrom fromIndex, next }
          , model.map !! fromIndex >>= _.army
              <#> \army ->
                  div []
                    [ h1 [] [ text "Army Units" ]
                    , ul []
                        $ army.units
                        <#> (\unit -> li [] [ text $ show unit.moves <> ": " <> show unit.unitType ])
                        # toArray
                    ]
          ]

boardClass :: ReactClass { model :: Model, regionHighlight :: RegionHighlight, next :: (Int -> Effect Unit) }
boardClass =
  statelessComponent \{ model, regionHighlight, next } ->
    div [ className "flex-column" ]
      [ createLeafElement mapHeader { header: "Turn: " <> show model.currentTurn }
      , svg
          [ className "map", width "658px", height "415px" ]
          $ cons
              (image [ href "assets/map_v2.png", width "658", height "415" ] [])
              ( concatMapWithIndex
                  ( \index region ->
                      catMaybes
                        [ region.army # map \army -> createLeafElement armyClass { region, index, side: army.side }
                        , Just
                            $ region.shape
                                ( catMaybes
                                    [ case regionHighlight of
                                        OwnedByPlayer -> do
                                          army <- region.army
                                          guard $ army.side == model.currentTurn
                                          pure (className "highlight green")
                                        DestinationsFrom fromIndex ->
                                          if index == fromIndex then
                                            Just (className "permenant yellow")
                                          else do
                                            guard $ canMove model.map fromIndex index
                                            pure $ className "permenant green"
                                    , Just $ key $ show index <> "-region"
                                    , Just $ onClick \e -> (logRegionClick e index) *> next index
                                    ]
                                )
                        ]
                  )
                  model.map
              )
      ]

-- data
initialBoard :: Board
initialBoard =
  -- 0
  [ { army: Just { side: Union, units: infantry : singleton cavalry }
    , shape: \shapeProps -> polygon (concat [ shapeProps, [ points "8,9 269,9 202,129 134,179 86,183 35,195 6,264" ] ]) []
    , position: { x: 80, y: 70 }
    , connections: Set.fromFoldable [ 1, 2, 12 ]
    }
  -- 1
  -- , { army: Just { side: Union, units: [] }
  , { army: Nothing
    , shape: \shapeProps -> polygon (concat [ shapeProps, [ points "268,10 346,9 348,211 234,210 202,131" ] ]) []
    , position: { x: 255, y: 105 }
    , connections: Set.fromFoldable [ 0, 2, 3, 4, 6 ]
    }
  -- 2
  -- , { army: Just { side: Union, units: [] }
  , { army: Nothing
    , shape: \shapeProps -> polygon (concat [ shapeProps, [ points "5,268 39,197 88,183 133,179 202,132 232,211 255,270" ] ]) []
    , position: { x: 100, y: 215 }
    , connections: Set.fromFoldable [ 0, 1, 3, 11, 12 ]
    }
  -- 3
  -- , { army: Just { side: Union, units: [] }
  , { army: Nothing
    , shape: \shapeProps -> polygon (concat [ shapeProps, [ points "233,211 348,210 347,270 257,270" ] ]) []
    , position: { x: 270, y: 220 }
    , connections: Set.fromFoldable [ 1, 2, 9, 10 ]
    }
  -- 4
  -- , { army: Just { side: Union, units: [] }
  , { army: Nothing
    , shape: \shapeProps -> polygon (concat [ shapeProps, [ points "349,9 527,6 524,88 512,134 347,135" ] ]) []
    , position: { x: 360, y: 15 }
    , connections: Set.fromFoldable [ 1, 5, 6, 13 ]
    }
  -- 5
  -- , { army: Just { side: Union, units: [] }
  , { army: Nothing
    , shape: \shapeProps -> polygon (concat [ shapeProps, [ points "529,8 652,9 653,133 512,132 526,86" ] ]) []
    , position: { x: 563, y: 53 }
    , connections: Set.fromFoldable [ 4, 7 ]
    }
  -- 6
  -- , { army: Just { side: Confederate, units: [] }
  , { army: Nothing
    , shape: \shapeProps -> polygon (concat [ shapeProps, [ points "349,134 514,135 494,166 462,201 432,216 380,215 350,210" ] ]) []
    , position: { x: 385, y: 153 }
    , connections: Set.fromFoldable [ 1, 4, 7, 9 ]
    }
  -- 7
  -- , { army: Just { side: Confederate, units: [] }
  , { army: Nothing
    , shape: \shapeProps -> polygon (concat [ shapeProps, [ points "514,134 651,134 653,256 632,259 563,307 523,312 486,282 483,242 464,203" ] ]) []
    , position: { x: 537, y: 193 }
    , connections: Set.fromFoldable [ 5, 6, 8, 9, 14 ]
    }
  -- 8
  -- , { army: Just { side: Confederate, units: [] }
  , { army: Nothing
    , shape: \shapeProps -> polygon (concat [ shapeProps, [ points "522,314 561,309 633,259 653,257 651,404 471,405 483,330" ] ]) []
    , position: { x: 545, y: 340 }
    , connections: Set.fromFoldable [ 7, 9, 14 ]
    }
  -- 9
  -- , { army: Just { side: Confederate, units: [] }
  , { army: Nothing
    , shape: \shapeProps -> polygon (concat [ shapeProps, [ points "347,211 388,215 435,215 461,205 482,244 485,280 482,331 472,406 349,406" ] ]) []
    , position: { x: 383, y: 290 }
    , connections: Set.fromFoldable [ 6, 7, 8, 14, 10, 3 ]
    }
  -- 10
  -- , { army: Just { side: Confederate, units: [] }
  , { army: Nothing
    , shape: \shapeProps -> polygon (concat [ shapeProps, [ points "258,270 346,271 349,408 259,408 249,385 264,300" ] ]) []
    , position: { x: 278, y: 325 }
    , connections: Set.fromFoldable [ 3, 9, 11 ]
    }
  -- 11
  -- , { army: Just { side: Confederate, units: [] }
  , { army: Nothing
    , shape: \shapeProps -> polygon (concat [ shapeProps, [ points "7,271 253,271 264,301 248,387 258,409 7,409" ] ]) []
    , position: { x: 30, y: 295 }
    , connections: Set.fromFoldable [ 2, 10, 15 ]
    }
  -- 12
  -- , { army: Just { side: Union, units: [] }
  , { army: Nothing
    , shape: \shapeProps -> circle (concat [ shapeProps, [ cx 107, cy 180, r 23 ] ]) []
    , position: { x: 83, y: 163 }
    , connections: Set.fromFoldable [ 0, 2 ]
    }
  -- 13
  -- , { army: Just { side: Union, units: [] }
  , { army: Nothing
    , shape: \shapeProps -> circle (concat [ shapeProps, [ cx 436, cy 70, r 24 ] ]) []
    , position: { x: 412, y: 53 }
    , connections: Set.fromFoldable [ 4 ]
    }
  -- 14
  -- , { army: Just { side: Confederate, units: [] }
  , { army: Nothing
    , shape: \shapeProps -> circle (concat [ shapeProps, [ cx 492, cy 305, r 24 ] ]) []
    , position: { x: 468, y: 287 }
    , connections: Set.fromFoldable [ 7, 8, 9 ]
    }
  -- 15
  -- , { army: Just { side: Confederate, units: [] }
  , { army: Nothing
    , shape: \shapeProps -> circle (concat [ shapeProps, [ cx 136, cy 345, r 21 ] ]) []
    , position: { x: 111, y: 328 }
    , connections: Set.fromFoldable [ 11 ]
    }
  ]
