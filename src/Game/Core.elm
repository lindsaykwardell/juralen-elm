module Game.Core exposing (CombatStatus(..), GameStatus(..), MobileTab(..), Model, PlayerStats, allCellsInRange, canAfford, currentPlayerStats, decoder, delay, encoder, getMoveCost, getPlayerRanking, getPlayerRankings, getPlayerScore, getPlayerTechTree, isInRange, playerAnalyzer)

import Game.AnalyzerMode exposing (AnalyzerMode)
import Game.Cell exposing (Cell)
import Game.CellType
import Game.Combat
import Game.Grid exposing (Grid)
import Game.History exposing (History)
import Game.Loc exposing (Loc)
import Game.Player exposing (Player)
import Game.PlayerScore exposing (PlayerScore)
import Game.Scenario
import Game.TechTree exposing (TechTree)
import Game.Unit exposing (Unit)
import Game.UnitType exposing (UnitType)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode
import Json.Encode as Encode
import Maybe.Extra as Maybe
import Process
import Sort.Dict
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
    | PurchaseTab
    | DetailsTab


type alias Model =
    { nextId : Int
    , turn : Int
    , grid : Grid
    , openCell : Loc
    , selectedCell : Loc
    , selectedUnits : List Int
    , players : List Player
    , activePlayer : Int
    , units : List Unit
    , scenario : Game.Scenario.Model
    , combat : CombatStatus
    , aiSpeed : Float
    , mobileTab : MobileTab
    , actionHistory : List History
    }


decoder : Decoder Model
decoder =
    Decode.succeed Model
        |> Decode.required "nextId" Decode.int
        |> Decode.required "turn" Decode.int
        |> Decode.required "grid" Game.Grid.decoder
        |> Decode.required "openCell" Game.Loc.decoder
        |> Decode.required "selectedCell" Game.Loc.decoder
        |> Decode.required "selectedUnits" (Decode.list Decode.int)
        |> Decode.required "players" (Decode.list Game.Player.decoder)
        |> Decode.required "activePlayer" Decode.int
        |> Decode.required "units" (Decode.list Game.Unit.decoder)
        |> Decode.required "scenario" Game.Scenario.decoder
        |> Decode.optional "combat" (Game.Combat.decoder |> Decode.andThen (\c -> Decode.succeed (Combat c))) NoCombat
        |> Decode.required "aiSpeed" Decode.float
        |> Decode.hardcoded UnitsTab
        |> Decode.required "actionHistory" (Decode.list Game.History.decoder)


encoder : Model -> Encode.Value
encoder model =
    Encode.object
        [ ( "nextId", Encode.int model.nextId )
        , ( "turn", Encode.int model.turn )
        , ( "grid", Game.Grid.encoder model.grid )
        , ( "openCell", Game.Loc.encoder model.openCell )
        , ( "selectedCell", Game.Loc.encoder model.selectedCell )
        , ( "selectedUnits", Encode.list Encode.int model.selectedUnits )
        , ( "players", Encode.list Game.Player.encoder model.players )
        , ( "activePlayer", Encode.int model.activePlayer )
        , ( "units", Encode.list Game.Unit.encoder model.units )
        , ( "scenario", Game.Scenario.encoder model.scenario )
        , ( "combat"
          , case model.combat of
                NoCombat ->
                    Encode.null

                Combat c ->
                    Game.Combat.encoder c
          )
        , ( "aiSpeed", Encode.float model.aiSpeed )
        , ( "actionHistory", Encode.list Game.History.encoder model.actionHistory )
        ]


type alias PlayerStats =
    { gold : Int
    , actions : Float
    , farms : () -> Int
    , towns : () -> Int
    , units : Int
    , techTree : TechTree
    }


currentPlayerStats : Model -> PlayerStats
currentPlayerStats model =
    playerStats model model.activePlayer


playerStats : Model -> Int -> PlayerStats
playerStats model playerId =
    { gold = Game.Player.get model.players playerId |> .resources |> .gold
    , actions = Game.Player.get model.players playerId |> .resources |> .actions
    , farms = \_ -> Game.Grid.farmCountControlledBy model.grid playerId
    , towns = \_ -> Game.Grid.townCountControlledBy model.grid playerId
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
    stats.farms () + stats.towns () + stats.units


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
    -- List.foldl
    --     (\id cost ->
    --         cost + Game.UnitType.moveCost (Game.Unit.fromId model.units id).unitType
    --     )
    --     0
    --     model.selectedUnits
    List.length model.selectedUnits |> toFloat


canAfford : Model -> UnitType -> Bool
canAfford model unitType =
    let
        stats =
            currentPlayerStats model

        unitCost =
            Game.UnitType.cost unitType
    in
    stats.units < stats.farms () && unitCost <= stats.gold


isInRange : Model -> Cell -> Bool
isInRange model cell =
    -- let
    --     isWizardSelected =
    --         List.any (\unitId -> (Game.Unit.fromId model.units unitId).unitType == Game.UnitType.Wizard) model.selectedUnits
    -- in
    List.length model.selectedUnits
        > 0
        && model.selectedCell
        /= cell.loc
        && Game.CellType.isPassable cell.cellType
        && (currentPlayerStats model).actions
        >= (Basics.toFloat (Game.Loc.getDistance model.selectedCell cell.loc) * getMoveCost model)
        && targetCellIsBordering model cell


allCellsInRange : Model -> List Cell
allCellsInRange model =
    let
        selectedLoc =
            model.selectedCell

        ( selectedX, selectedY ) =
            Game.Loc.coords selectedLoc

        allCellsFromDistance : Int -> List Cell -> List Cell
        allCellsFromDistance distance cells =
            allCellsAtDistance distance
                |> (\newCells ->
                        case newCells of
                            [] ->
                                cells

                            _ ->
                                allCellsFromDistance (distance + 1) (newCells ++ cells)
                   )

        allCellsAtDistance : Int -> List Cell
        allCellsAtDistance distance =
            let
                minX =
                    selectedX - distance

                maxX =
                    selectedX + distance

                minY =
                    selectedY - distance

                maxY =
                    selectedY + distance

                xRange =
                    List.range minX maxX

                yRange =
                    List.range minY maxY

                locsAtDistance : List Loc
                locsAtDistance =
                    List.concat
                        [ List.map (\y -> Game.Loc.at minX y) yRange
                        , List.map (\y -> Game.Loc.at maxX y) yRange
                        , List.map (\x -> Game.Loc.at x minY) xRange
                        , List.map (\x -> Game.Loc.at x maxY) xRange
                        ]
            in
            locsAtDistance
                |> List.filterMap (\loc -> Sort.Dict.get loc model.grid)
                |> List.filter (isInRange model)
    in
    allCellsFromDistance 1 []


delay : Float -> msg -> Cmd msg
delay time msg =
    Process.sleep time
        |> Task.perform (\_ -> msg)


targetCellIsBordering : Model -> Cell -> Bool
targetCellIsBordering model cell =
    let
        borderingCells : List Cell
        borderingCells =
            Game.Grid.getBorderCells model.grid cell.loc
    in
    List.filter
        (\borderCell ->
            case borderCell.controlledBy of
                Just playerId ->
                    let
                        borderingForests : List Cell
                        borderingForests =
                            Game.Grid.getBorderCells model.grid cell.loc
                                |> List.filter
                                    (.cellType >> (==) Game.CellType.Forest)

                        borderingPlayers =
                            List.foldl
                                (\f players ->
                                    (model.grid
                                        |> Game.Grid.ofType Game.CellType.Forest
                                        |> Game.Grid.getGroups
                                        |> Game.Grid.getGroupBorderingPlayers model.grid f.loc
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
                    case borderCell.cellType of
                        Game.CellType.Mountain ->
                            False

                        -- Game.CellType.Plains ->
                        --     True
                        _ ->
                            let
                                borderingPlayers =
                                    model.grid
                                        |> Game.Grid.ofType borderCell.cellType
                                        |> Game.Grid.getGroups
                                        |> Game.Grid.getGroupBorderingPlayers model.grid cell.loc
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
         -- False
        )
        borderingCells
        /= []
