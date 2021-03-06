module Game.Core exposing (..)

import Game.Combat
import Juralen.Analysis exposing (Option)
import Juralen.Cell exposing (Cell, Loc)
import Juralen.CellType exposing (CellType)
import Juralen.Grid exposing (Grid)
import Juralen.Player exposing (NewPlayer, Player)
import Juralen.Unit exposing (Unit)
import Juralen.UnitType exposing (UnitType)
import Process
import Task
import Juralen.AnalyzerMode exposing (AnalyzerMode)
import Juralen.TechTree exposing (TechTree)

type GameStatus
    = NoGame
    | ActiveGame
    | CompletedGame

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
    , analysisResults : List Option
    , aiSpeed : Float
    }


type alias PlayerStats =
    { gold : Int
    , actions : Float
    , farms : Int
    , towns : Int
    , units : Int
    , techTree : TechTree
    }


currentPlayerStats : Model -> PlayerStats
currentPlayerStats model = playerStats model model.activePlayer

playerStats : Model -> Int -> PlayerStats
playerStats model playerId =
    { gold = (Juralen.Player.getResources model.players playerId).gold
    , actions = (Juralen.Player.getResources model.players playerId).actions
    , farms = Juralen.Grid.farmCountControlledBy model.grid playerId
    , towns = Juralen.Grid.townCountControlledBy model.grid playerId
    , units = List.length (List.filter (\unit -> unit.controlledBy == playerId) model.units)
    , techTree = getPlayerTechTree model.players playerId
    }

playerAnalyzer : List Player -> Int -> AnalyzerMode
playerAnalyzer players playerId =
    case players of
        (player :: remainingPlayers) ->
            if player.id == playerId then
                player.analyzer
            
            else
                playerAnalyzer remainingPlayers playerId

        [] ->
            Juralen.AnalyzerMode.Default

getPlayerScore : Model -> Int -> Int
getPlayerScore model playerId =
    let
        stats : PlayerStats
        stats = playerStats model playerId
    in
        stats.farms + stats.towns + stats.units

type alias PlayerScore =
    { playerId : Int
    , score : Int }

getPlayerRankings : Model -> List PlayerScore
getPlayerRankings model =
    List.map (\player -> { playerId = player.id, score = getPlayerScore model player.id }) model.players    

getPlayerRanking : List PlayerScore -> Int -> Int -> Int
getPlayerRanking playerScores playerId rank =
    case playerScores of
        (score :: scores) ->
            if score.playerId == playerId then rank 
            else getPlayerRanking scores playerId rank + 1

        [] ->
            -1
    

getPlayerTechTree : List Player -> Int -> TechTree
getPlayerTechTree players playerId =
    case players of 
        (player :: remainingPlayers) ->
            if player.id == playerId then player.techTree

            else getPlayerTechTree remainingPlayers playerId
        [] ->
            Juralen.TechTree.empty

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


delay : Float -> msg -> Cmd msg
delay time msg =
    Process.sleep time
        |> Task.perform (\_ -> msg)
