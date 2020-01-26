module Juralen.Unit exposing (..)

import Juralen.Cell exposing (Cell, Loc)
import Juralen.Player exposing (Player)
import Juralen.UnitType exposing (UnitType(..))


type alias Unit =
    { id : Int
    , unitType : UnitType
    , movesLeft : Int
    , attack : Int
    , health : Int
    , range : Int
    , controlledBy : Int
    , x : Int
    , y : Int
    }


buildUnit : UnitType -> Int -> Loc -> Int -> Unit
buildUnit unitType playerId loc id =
    let
        initialValues =
            Juralen.UnitType.initialValues unitType
    in
    { id = id
    , unitType = unitType
    , movesLeft = initialValues.movesLeft
    , attack = initialValues.attack
    , health = initialValues.health
    , range = initialValues.range
    , controlledBy = playerId
    , x = loc.x
    , y = loc.y
    }


inCell : List Unit -> Loc -> List Unit
inCell units loc =
    List.filter (\unit -> unit.x == loc.x && unit.y == loc.y) units
