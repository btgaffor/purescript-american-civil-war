module Army.Models where

import Prelude
import Data.Array.NonEmpty (NonEmptyArray)

type Army
  = { side :: Side
    , units :: NonEmptyArray ArmyUnit
    }

data UnitType
  = Infantry
  | Cavalry
  | EliteCavalry
  | Artillary
  | Leader

instance showUnitType :: Show UnitType where
  show Infantry = "Infantry"
  show Cavalry = "Cavalry"
  show EliteCavalry = "EliteCavalry"
  show Artillary = "Artillary"
  show Leader = "Leader"

type ArmyUnit
  = { unitType :: UnitType, moves :: Int }

infantry :: ArmyUnit
infantry = { unitType: Infantry, moves: 1 }

cavalry :: ArmyUnit
cavalry = { unitType: Cavalry, moves: 2 }

data Side
  = Union
  | Confederate

otherSide :: Side -> Side
otherSide side
  | side == Union = Confederate
  | otherwise = Union

resetArmyMoves :: ArmyUnit -> ArmyUnit
resetArmyMoves armyUnit = case armyUnit.unitType of
  Infantry -> armyUnit { moves = 1 }
  Cavalry -> armyUnit { moves = 2 }
  EliteCavalry -> armyUnit { moves = 2 }
  Artillary -> armyUnit { moves = 1 }
  Leader -> armyUnit { moves = 2 }

instance showSide :: Show Side where
  show Union = "Union"
  show Confederate = "Confederate"

derive instance eqSide :: Eq Side
