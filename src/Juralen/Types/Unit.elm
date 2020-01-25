module Juralen.Types.Unit exposing (..)

import Juralen.Types.Cell exposing (Cell)
import Juralen.Types.Loc exposing (Loc)
import Juralen.Types.Player exposing (Player)


type alias Unit =
    { id : Int
    , name : String
    , short : String
    , cost : Int
    , move : Float
    , movesLeft : Int
    , maxMoves : Int
    , attack : Int
    , health : Int
    , maxHealth : Int
    , range : Int
    , description : String
    , controlledBy : Int
    , x : Int
    , y : Int
    }


findUnitsInCell : List Unit -> Cell -> List Unit
findUnitsInCell units cell =
    List.filter (\unit -> unit.x == cell.x && unit.y == cell.y) units


buildSoldier : Player -> Loc -> Int -> Unit
buildSoldier player loc id =
    { id = id
    , name = "Soldier"
    , short = "So"
    , cost = 1
    , move = 1
    , movesLeft = 1
    , maxMoves = 1
    , attack = 1
    , health = 2
    , maxHealth = 2
    , range = 1
    , description = "Common unit. Best at defense in a structure"
    , controlledBy = player.id
    , x = loc.x
    , y = loc.y
    }
