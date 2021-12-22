module Game.Unit exposing (..)

import Game.Loc as Loc exposing (Loc)
import Game.UnitType exposing (UnitType(..))


type alias Unit =
    { id : Int
    , unitType : UnitType
    , movesLeft : Int
    , attack : Int
    , health : Int
    , range : Int
    , controlledBy : Int
    , loc : Loc
    }


buildUnit : UnitType -> Int -> Loc -> Int -> Unit
buildUnit unitType playerId loc id =
    let
        initialValues =
            Game.UnitType.initialValues unitType
    in
    { id = id
    , unitType = unitType
    , movesLeft = initialValues.movesLeft
    , attack = initialValues.attack
    , health = initialValues.health
    , range = initialValues.range
    , controlledBy = playerId
    , loc = loc
    }


inCell : List Unit -> Loc -> List Unit
inCell units loc =
    List.filter (\unit -> unit.loc == loc) units


fromId : List Unit -> Int -> Unit
fromId units id =
    case List.head (List.filter (\unit -> unit.id == id) units) of
        Nothing ->
            empty

        Just unit ->
            unit


isSelected : List Int -> Int -> Bool
isSelected selectedUnits unitId =
    case List.head (List.filter (\id -> id == unitId) selectedUnits) of
        Nothing ->
            False

        _ ->
            True


controlledBy : List Unit -> Int -> List Unit
controlledBy units playerId =
    List.filter (\unit -> unit.controlledBy == playerId) units


empty : Unit
empty =
    { id = -1
    , unitType = Game.UnitType.Soldier
    , movesLeft = -1
    , attack = 0
    , health = -1
    , range = -1
    , controlledBy = -1
    , loc = Loc.at -1 -1
    }


takeDamage : Unit -> Int -> Unit
takeDamage unit damage =
    { unit | health = unit.health - damage }


isDead : Unit -> Bool
isDead unit =
    unit.health <= 0
