module Game.Core exposing (..)

import Game.Analysis exposing (Option)
import Game.AnalyzerMode exposing (AnalyzerMode)
import Game.Cell exposing (Cell, Loc, getBorderCells, getGroupBorderingPlayers, groupNeighbors, ofType)
import Game.CellType
import Game.Combat
import Game.Grid exposing (Grid)
import Game.Player exposing (NewPlayer, Player)
import Game.TechTree exposing (TechTree)
import Game.Unit exposing (Unit)
import Game.UnitType exposing (UnitType)
import Process
import Task


type GameStatus
    = NoGame
    | ActiveGame
    | CompletedGame


type CombatStatus
    = NoCombat
    | Combat Game.Combat.Model


type MobileTab
    = UnitsTab
    | TechTreeTab
    | BuildOptionsTab
    | DetailsTab


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
    , mobileTab : MobileTab
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
currentPlayerStats model =
    playerStats model model.activePlayer


playerStats : Model -> Int -> PlayerStats
playerStats model playerId =
    { gold = (Game.Player.getResources model.players playerId).gold
    , actions = (Game.Player.getResources model.players playerId).actions
    , farms = Game.Grid.farmCountControlledBy model.grid playerId
    , towns = Game.Grid.townCountControlledBy model.grid playerId
    , units = List.length (List.filter (\unit -> unit.controlledBy == playerId) model.units)
    , techTree = getPlayerTechTree model.players playerId
    }


playerAnalyzer : List Player -> Int -> AnalyzerMode
playerAnalyzer players playerId =
    case players of
        player :: remainingPlayers ->
            if player.id == playerId then
                player.analyzer

            else
                playerAnalyzer remainingPlayers playerId

        [] ->
            Game.AnalyzerMode.Default


getPlayerScore : Model -> Int -> Int
getPlayerScore model playerId =
    let
        stats : PlayerStats
        stats =
            playerStats model playerId
    in
    stats.farms + stats.towns + stats.units


type alias PlayerScore =
    { playerId : Int
    , score : Int
    }


getPlayerRankings : Model -> List PlayerScore
getPlayerRankings model =
    List.map (\player -> { playerId = player.id, score = getPlayerScore model player.id }) model.players


getPlayerRanking : List PlayerScore -> Int -> Int -> Int
getPlayerRanking playerScores playerId rank =
    case playerScores of
        score :: scores ->
            if score.playerId == playerId then
                rank

            else
                getPlayerRanking scores playerId rank + 1

        [] ->
            -1


getPlayerTechTree : List Player -> Int -> TechTree
getPlayerTechTree players playerId =
    case players of
        player :: remainingPlayers ->
            if player.id == playerId then
                player.techTree

            else
                getPlayerTechTree remainingPlayers playerId

        [] ->
            Game.TechTree.empty


getMoveCost : Model -> Float
getMoveCost model =
    List.foldl
        (\id cost ->
            cost + Game.UnitType.moveCost (Game.Unit.fromId model.units id).unitType
        )
        0
        model.selectedUnits


canAfford : Model -> UnitType -> Bool
canAfford model unitType =
    let
        stats =
            currentPlayerStats model

        unitCost =
            Game.UnitType.cost unitType
    in
    stats.units < stats.farms && unitCost <= stats.gold


isInRange : Model -> Cell -> Bool
isInRange model cell =
    -- let
    --     isWizardSelected =
    --         List.any (\unitId -> (Game.Unit.fromId model.units unitId).unitType == Game.UnitType.Wizard) model.selectedUnits
    -- in
    List.length model.selectedUnits
        > 0
        && model.selectedCell
        /= { x = cell.x, y = cell.y }
        && Game.CellType.isPassable cell.cellType
        && (currentPlayerStats model).actions
        >= (Basics.toFloat (Game.Cell.getDistance model.selectedCell { x = cell.x, y = cell.y }) * getMoveCost model)
        && targetCellIsBordering model cell


allCellsInRange : Model -> List Cell
allCellsInRange model =
    List.foldl
        (\row collection ->
            List.foldl
                (\cell cells ->
                    if isInRange model cell then
                        cell :: cells

                    else
                        cells
                )
                collection
                row
        )
        []
        model.grid


delay : Float -> msg -> Cmd msg
delay time msg =
    Process.sleep time
        |> Task.perform (\_ -> msg)


targetCellIsBordering : Model -> Cell -> Bool
targetCellIsBordering model cell =
    let
        borderingCells : List (Maybe Cell)
        borderingCells =
            getBorderCells model.grid { x = cell.x, y = cell.y }
    in
    List.filter
        (\borderCell ->
            case borderCell of
                Nothing ->
                    False

                Just c ->
                    case c.controlledBy of
                        Just playerId ->
                            let
                                borderingForests : List (Maybe Cell)
                                borderingForests =
                                    getBorderCells model.grid { x = cell.x, y = cell.y }
                                        |> List.filter
                                            (\maybeCell ->
                                                case maybeCell of
                                                    Nothing ->
                                                        False

                                                    Just f ->
                                                        f.cellType == Game.CellType.Forest
                                            )

                                borderingPlayers =
                                    List.foldl
                                        (\maybeCell players ->
                                            case maybeCell of
                                                Nothing ->
                                                    players

                                                Just f ->
                                                    (model.grid
                                                        |> ofType Game.CellType.Forest
                                                        |> groupNeighbors
                                                        |> getGroupBorderingPlayers model.grid { x = f.x, y = f.y }
                                                    )
                                                        ++ players
                                        )
                                        []
                                        borderingForests
                            in
                            playerId
                                == model.activePlayer
                                || List.any
                                    (\player ->
                                        case player of
                                            Just pid ->
                                                pid == model.activePlayer

                                            Nothing ->
                                                False
                                    )
                                    borderingPlayers

                        Nothing ->
                            case c.cellType of
                                Game.CellType.Mountain ->
                                    False

                                Game.CellType.Plains ->
                                    True

                                _ ->
                                    let
                                        borderingPlayers =
                                            model.grid
                                                |> ofType c.cellType
                                                |> groupNeighbors
                                                |> getGroupBorderingPlayers model.grid { x = cell.x, y = cell.y }
                                    in
                                    List.any
                                        (\player ->
                                            case player of
                                                Just playerId ->
                                                    playerId == model.activePlayer

                                                Nothing ->
                                                    False
                                        )
                                        borderingPlayers
        )
        borderingCells
        /= []
