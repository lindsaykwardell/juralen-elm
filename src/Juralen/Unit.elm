module Juralen.Unit exposing (..)

import Juralen.Cell exposing (Loc)
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

fromId : List Unit -> Int -> Unit
fromId units id =
    case List.head (List.filter (\unit -> unit.id == id) units) of
        Nothing ->
            {id = -1
            , unitType = Juralen.UnitType.Soldier
            , movesLeft = -1
            , attack = 0
            , health = -1
            , range = -1
            , controlledBy = -1
            , x = -1
            , y = -1}

        Just unit ->
            unit



isSelected : List Int -> Int -> Bool
isSelected selectedUnits unitId =
    case List.head (List.filter (\id -> id == unitId) selectedUnits) of
        Nothing ->
            False

        _ ->
            True