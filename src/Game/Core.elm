module Game.Core exposing (..)

import Game.Combat
import Juralen.Cell exposing (Cell, Loc)
import Juralen.CellType exposing (CellType)
import Juralen.Grid exposing (Grid)
import Juralen.Player exposing (NewPlayer, Player)
import Juralen.Unit exposing (Unit)
import Juralen.UnitType exposing (UnitType)


type CombatStatus
    = NoCombat
    | Combat Game.Combat.Model


type alias Model =
    { nextId : Int
    , grid : Grid
    , selectedCell : Loc
    , selectedUnits : List Int
    , players : List Player
    , activePlayer : Int
    , units : List Unit
    , init :
        { maxX : Int
        , maxY : Int
        , currentX : Int
        , currentY : Int
        , finished : Bool
        , newPlayers : List NewPlayer
        }
    , combat : CombatStatus
    }


type alias CurrentPlayerStats =
    { gold : Int
    , actions : Float
    , farms : Int
    , towns : Int
    , units : Int
    }


currentPlayerStats : Model -> CurrentPlayerStats
currentPlayerStats model =
    { gold = (Juralen.Player.getResources model.players model.activePlayer).gold
    , actions = (Juralen.Player.getResources model.players model.activePlayer).actions
    , farms = Juralen.Grid.farmCountControlledBy model.grid model.activePlayer
    , towns = Juralen.Grid.townCountControlledBy model.grid model.activePlayer
    , units = List.length (List.filter (\unit -> unit.controlledBy == model.activePlayer) model.units)
    }


getMoveCost : Model -> Float
getMoveCost model =
    List.foldl
        (\id cost ->
            cost + Juralen.UnitType.moveCost (Juralen.Unit.fromId model.units id).unitType
        )
        0
        model.selectedUnits


canAfford : Model -> UnitType -> Bool
canAfford model unitType =
    let
        stats =
            currentPlayerStats model

        unitCost =
            Juralen.UnitType.cost unitType
    in
    stats.units < stats.farms && unitCost <= stats.gold


isInRange : Model -> Cell -> Bool
isInRange model cell =
    List.length model.selectedUnits
        > 0
        && model.selectedCell
        /= { x = cell.x, y = cell.y }
        && Juralen.CellType.isPassable cell.cellType
        && (currentPlayerStats model).actions
        >= (Basics.toFloat (Juralen.Cell.getDistance model.selectedCell { x = cell.x, y = cell.y }) * getMoveCost model)
