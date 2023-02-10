port module Game.Update exposing (Msg(..), update)

import Game.Action as Action
import Game.Analyzer exposing (analyze)
import Game.Cell exposing (Cell)
import Game.CellType
import Game.Combat
import Game.Core as Core exposing (..)
import Game.Grid exposing (Grid)
import Game.History exposing (History)
import Game.Loc as Loc exposing (Loc)
import Game.Option as Option
import Game.Player exposing (Player)
import Game.Resources exposing (Resources)
import Game.Scenario exposing (Msg)
import Game.Structure
import Game.TechTree as TechTree exposing (TechDescription)
import Game.Unit exposing (Unit)
import Game.UnitType exposing (UnitType)
import Json.Encode as Encode
import Sort.Dict


port analyze : String -> Cmd msg


type Msg
    = InitializeScenario
    | GotScenarioMsg Game.Scenario.Msg
    | StartTurn Player
    | SelectCell Loc
    | BuildUnit UnitType
    | SelectUnit Int
    | MoveSelectedUnits Cell
    | ResearchTech TechDescription
    | UpgradeCell Action.UpgradeType
    | GotCombatMsg Game.Combat.Msg
    | PerformAction Option.Option
    | PerformAiTurn
    | EndTurn
    | EndGame
    | SaveGame
    | OpenSettings
    | UpdateMobileTab MobileTab
    | ZoomIn
    | ZoomOut
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InitializeScenario ->
            update (GotScenarioMsg Game.Scenario.InitializeScenario) model

        GotScenarioMsg scenarioMsg ->
            let
                scenario : Game.Scenario.Model
                scenario =
                    model.scenario
            in
            case scenarioMsg of
                Game.Scenario.ScenarioLoaded ->
                    let
                        firstPlayer : Player
                        firstPlayer =
                            Game.Player.get scenario.players scenario.activePlayerId
                    in
                    update (StartTurn firstPlayer)
                        { model
                            | activePlayer = scenario.activePlayerId
                            , nextId = scenario.nextId
                            , players = scenario.players
                            , units = scenario.units
                            , grid = scenario.grid
                        }

                _ ->
                    toScenario model (Game.Scenario.update scenarioMsg scenario)

        StartTurn nextActivePlayer ->
            if
                Game.Scenario.isEndConditionReached
                    { players = model.players
                    , nextActivePlayer = nextActivePlayer
                    , units = model.units
                    , getPlayerScore = Core.getPlayerScore model
                    , turn = model.turn
                    , townCountControlledBy = Game.Grid.townCountControlledBy model.grid
                    }
                    model.scenario
            then
                update EndGame { model | activePlayer = -1 }

            else
                let
                    updatedPlayers : List Player
                    updatedPlayers =
                        List.map
                            (\player ->
                                if player.id == nextActivePlayer.id then
                                    { nextActivePlayer | resources = gainResources (currentPlayerStats model) nextActivePlayer.resources }

                                else
                                    player
                            )
                            model.players

                    updatedUnits =
                        List.map
                            (\unit ->
                                { unit
                                    | movesLeft = unit.maxMoves
                                }
                            )
                            model.units

                    newModel =
                        { model
                            | turn = model.turn + 1
                            , activePlayer = nextActivePlayer.id
                            , players = updatedPlayers
                            , units = updatedUnits
                            , selectedCell =
                                if Game.Player.get model.players model.activePlayer |> .isHuman then
                                    model.openCell

                                else
                                    model.selectedCell
                        }
                in
                ( newModel, runAiAction newModel )

        SelectCell loc ->
            ( { model
                | openCell = loc
                , selectedCell =
                    if Game.Player.get model.players model.activePlayer |> .isHuman then
                        loc

                    else
                        model.selectedCell
                , selectedUnits = []
              }
            , Cmd.none
            )

        BuildUnit unitType ->
            let
                newResources : Resources
                newResources =
                    Game.Resources.spend (Game.Player.get model.players model.activePlayer |> .resources) (Game.UnitType.cost unitType)

                newPlayerList =
                    List.map
                        (\player ->
                            if player.id == model.activePlayer then
                                { player | resources = newResources }

                            else
                                player
                        )
                        model.players
            in
            if canAfford model unitType then
                let
                    newUnit : Unit
                    newUnit =
                        Game.Unit.buildUnit unitType model.activePlayer model.selectedCell model.nextId

                    newUnitList =
                        model.units ++ [ newUnit ]

                    nextId : Int
                    nextId =
                        model.nextId + 1

                    newModel =
                        { model | nextId = nextId, units = newUnitList, players = newPlayerList }
                            |> recordAction
                                { player = Game.Player.get model.players model.activePlayer |> .name
                                , turn = model.turn
                                , loc = model.selectedCell
                                , action = Action.BuildUnit unitType
                                , scores = Core.getPlayerRankings model
                                }
                in
                ( newModel, runAiAction newModel )

            else
                ( model, Cmd.none )

        SelectUnit unitId ->
            if (Game.Unit.fromId model.units unitId).controlledBy /= model.activePlayer || (Game.Unit.fromId model.units unitId).movesLeft <= 0 then
                ( model, Cmd.none )

            else
                case List.head (List.filter (\id -> id == unitId) model.selectedUnits) of
                    Nothing ->
                        let
                            newSelectedUnitList =
                                model.selectedUnits ++ [ unitId ]
                        in
                        ( { model | selectedUnits = newSelectedUnitList }, Cmd.none )

                    _ ->
                        let
                            newSelectedUnitList =
                                List.filter (\id -> id /= unitId) model.selectedUnits
                        in
                        ( { model | selectedUnits = newSelectedUnitList }, Cmd.none )

        MoveSelectedUnits cell ->
            if
                Game.Grid.isInRange
                    { selectedUnits = model.selectedUnits
                    , selectedCell = model.selectedCell
                    , actions = currentPlayerStats model |> .actions
                    , moveCost = getMoveCost model
                    , grid = model.grid
                    , activePlayer = model.activePlayer
                    }
                    cell
                    == False
            then
                ( model, Cmd.none )

            else
                let
                    newResources : Resources
                    newResources =
                        Game.Resources.useActions
                            (Game.Player.get model.players model.activePlayer |> .resources)
                            (Basics.toFloat (Loc.getDistance model.selectedCell cell.loc) * getMoveCost model)

                    newPlayerList =
                        List.map
                            (\player ->
                                if player.id == model.activePlayer then
                                    { player | resources = newResources }

                                else
                                    player
                            )
                            model.players

                    newUnitList =
                        List.map
                            (\unit ->
                                if
                                    Game.Unit.isSelected model.selectedUnits unit.id
                                        && unit.loc
                                        == model.selectedCell
                                then
                                    { unit | loc = cell.loc, movesLeft = unit.movesLeft - 1 }

                                else
                                    unit
                            )
                            model.units

                    newSelectedCell =
                        cell.loc

                    newCell =
                        if
                            cell.cellType
                                == Game.CellType.Plains
                                && List.length (Game.Unit.inCell model.units cell.loc)
                                <= 0
                        then
                            Game.Cell.updateControl cell model.activePlayer

                        else
                            cell

                    newGrid =
                        Game.Grid.replaceCell model.grid newCell

                    initCombat =
                        shouldCombatStart (Game.Unit.inCell newModel.units newModel.selectedCell) []

                    newModel =
                        { model
                            | grid = newGrid
                            , players = newPlayerList
                            , units = newUnitList
                            , selectedCell = newSelectedCell
                            , openCell =
                                if Game.Player.get model.players model.activePlayer |> .isHuman then
                                    newSelectedCell

                                else
                                    model.openCell
                            , selectedUnits = []
                        }

                    modelWithRecordedAction =
                        newModel
                            |> recordAction
                                { player = Game.Player.get model.players model.activePlayer |> .name
                                , turn = model.turn
                                , loc = model.selectedCell
                                , action =
                                    (if initCombat then
                                        Action.Attack

                                     else
                                        Action.Move
                                    )
                                        (List.filter
                                            (\unit ->
                                                Game.Unit.isSelected model.selectedUnits unit.id
                                            )
                                            model.units
                                        )
                                        cell.loc
                                , scores = Core.getPlayerRankings model
                                }
                in
                if initCombat then
                    let
                        combatModel : Game.Combat.Model
                        combatModel =
                            { units = Game.Unit.inCell modelWithRecordedAction.units modelWithRecordedAction.selectedCell
                            , deadUnits = []
                            , attacker = Game.Unit.empty
                            , defender = Game.Unit.empty
                            , attackingPlayer = Game.Player.get modelWithRecordedAction.players modelWithRecordedAction.activePlayer
                            , defendingPlayer =
                                case Sort.Dict.get modelWithRecordedAction.selectedCell modelWithRecordedAction.grid of
                                    Nothing ->
                                        Game.Player.empty

                                    Just selectedCell ->
                                        case selectedCell.controlledBy of
                                            Nothing ->
                                                Game.Player.empty

                                            Just defendingPlayerId ->
                                                Game.Player.get modelWithRecordedAction.players defendingPlayerId
                            , whoGoesFirst = Game.Combat.Attacker
                            , defBonus = cell.defBonus
                            , cell =
                                Sort.Dict.get modelWithRecordedAction.selectedCell modelWithRecordedAction.grid
                                    |> Maybe.withDefault Game.Cell.empty
                            }

                        startCombat : CombatStatus
                        startCombat =
                            Combat combatModel
                    in
                    update (GotCombatMsg (Game.Combat.GetRandomUnit Game.Combat.Attacker)) { modelWithRecordedAction | combat = startCombat }

                else
                    ( modelWithRecordedAction, runAiAction modelWithRecordedAction )

        ResearchTech tech ->
            let
                stats =
                    Core.currentPlayerStats model

                cost =
                    tech.cost
            in
            if stats.gold < cost then
                ( model, Cmd.none )

            else
                let
                    techTree =
                        TechTree.research (Core.getPlayerTechTree model.players model.activePlayer) tech.tech

                    players =
                        List.map
                            (\player ->
                                if player.id == model.activePlayer then
                                    { player | techTree = techTree, resources = Game.Resources.spend player.resources tech.cost }

                                else
                                    player
                            )
                            model.players

                    newModel =
                        { model | players = players }
                            |> recordAction
                                { player = Game.Player.get model.players model.activePlayer |> .name
                                , turn = model.turn
                                , loc = model.selectedCell
                                , action = Action.Research tech
                                , scores = Core.getPlayerRankings model
                                }
                in
                ( newModel, runAiAction newModel )

        UpgradeCell upgradeType ->
            let
                stats =
                    Core.currentPlayerStats model
            in
            case upgradeType of
                Action.BuildFarm ->
                    if stats.gold < 2 then
                        ( model, Cmd.none )

                    else
                        let
                            players =
                                List.map
                                    (\player ->
                                        if player.id == model.activePlayer then
                                            { player | resources = Game.Resources.spend player.resources 2 }

                                        else
                                            player
                                    )
                                    model.players

                            cell : Maybe Cell
                            cell =
                                Sort.Dict.get model.selectedCell model.grid

                            grid : Grid
                            grid =
                                case cell of
                                    Nothing ->
                                        model.grid

                                    Just aCell ->
                                        Game.Grid.replaceCell model.grid { aCell | farms = aCell.farms + 1 }

                            newModel =
                                { model | players = players, grid = grid }
                                    |> recordAction
                                        { player = Game.Player.get model.players model.activePlayer |> .name
                                        , turn = model.turn
                                        , loc = model.selectedCell
                                        , action = Action.Upgrade Action.BuildFarm
                                        , scores = Core.getPlayerRankings model
                                        }
                        in
                        ( newModel, runAiAction newModel )

                Action.BuildTower ->
                    if stats.gold < 2 then
                        ( model, Cmd.none )

                    else
                        let
                            players =
                                List.map
                                    (\player ->
                                        if player.id == model.activePlayer then
                                            { player | resources = Game.Resources.spend player.resources 2 }

                                        else
                                            player
                                    )
                                    model.players

                            cell : Maybe Cell
                            cell =
                                Sort.Dict.get model.selectedCell model.grid

                            grid : Grid
                            grid =
                                case cell of
                                    Nothing ->
                                        model.grid

                                    Just aCell ->
                                        Game.Grid.replaceCell model.grid { aCell | towers = aCell.towers + 1 }

                            newModel =
                                { model | players = players, grid = grid }
                                    |> recordAction
                                        { player = Game.Player.get model.players model.activePlayer |> .name
                                        , turn = model.turn
                                        , loc = model.selectedCell
                                        , action = Action.Upgrade Action.BuildTower
                                        , scores = Core.getPlayerRankings model
                                        }
                        in
                        ( newModel, runAiAction newModel )

                Action.RepairDefense ->
                    if stats.gold < 1 then
                        ( model, Cmd.none )

                    else
                        let
                            players =
                                List.map
                                    (\player ->
                                        if player.id == model.activePlayer then
                                            { player | resources = Game.Resources.spend player.resources 1 }

                                        else
                                            player
                                    )
                                    model.players

                            cell : Maybe Cell
                            cell =
                                Sort.Dict.get model.selectedCell model.grid

                            grid : Grid
                            grid =
                                case cell of
                                    Nothing ->
                                        model.grid

                                    Just aCell ->
                                        Game.Grid.replaceCell model.grid
                                            { aCell
                                                | defBonus =
                                                    if Game.Structure.initDef aCell.structure > aCell.defBonus then
                                                        aCell.defBonus + 1

                                                    else
                                                        aCell.defBonus
                                            }

                            newModel =
                                { model | players = players, grid = grid }
                                    |> recordAction
                                        { player = Game.Player.get model.players model.activePlayer |> .name
                                        , turn = model.turn
                                        , loc = model.selectedCell
                                        , action = Action.Upgrade Action.RepairDefense
                                        , scores = Core.getPlayerRankings model
                                        }
                        in
                        ( newModel, runAiAction newModel )

        GotCombatMsg combatMsg ->
            case model.combat of
                NoCombat ->
                    ( model, Cmd.none )

                Combat combat ->
                    case combatMsg of
                        Game.Combat.ExitCombat ->
                            let
                                deadUnitIds =
                                    List.map (\unit -> unit.id) combat.deadUnits

                                newUnits : List Unit
                                newUnits =
                                    List.filter
                                        (\unit ->
                                            not (List.member unit.id deadUnitIds)
                                        )
                                        (List.map
                                            (\unit ->
                                                case
                                                    List.head
                                                        (List.filter (\livingUnit -> livingUnit.id == unit.id) combat.units)
                                                of
                                                    Nothing ->
                                                        unit

                                                    Just livingUnit ->
                                                        livingUnit
                                            )
                                            model.units
                                        )

                                winner =
                                    if List.length (Game.Unit.controlledBy combat.units combat.attackingPlayer.id) > 0 then
                                        combat.attackingPlayer

                                    else
                                        combat.defendingPlayer

                                updatedGrid : Grid
                                updatedGrid =
                                    Sort.Dict.update model.selectedCell
                                        (Maybe.map
                                            (\cell ->
                                                let
                                                    conquered =
                                                        cell.controlledBy /= Just winner.id
                                                in
                                                { cell
                                                    | controlledBy =
                                                        if cell.cellType == Game.CellType.Forest then
                                                            Nothing

                                                        else
                                                            Just winner.id
                                                    , farms =
                                                        if conquered then
                                                            0

                                                        else
                                                            cell.farms
                                                    , towers =
                                                        if conquered then
                                                            0

                                                        else
                                                            cell.towers
                                                    , defBonus =
                                                        if combat.defBonus < 0 then
                                                            0

                                                        else
                                                            combat.defBonus
                                                }
                                            )
                                        )
                                        model.grid
                            in
                            ( { model
                                | units = newUnits
                                , grid = updatedGrid
                                , combat = NoCombat
                              }
                            , runAiAction
                                { model
                                    | units = newUnits
                                    , grid = updatedGrid
                                }
                            )

                        _ ->
                            toCombat model (Game.Combat.update combatMsg combat)

        PerformAction option ->
            case option.action of
                Action.Move units toLoc ->
                    let
                        selectedUnits =
                            List.map (\unit -> unit.id) units

                        toCell =
                            Sort.Dict.get toLoc model.grid
                    in
                    case toCell of
                        Nothing ->
                            ( model, Cmd.none )

                        Just cell ->
                            update (MoveSelectedUnits cell) { model | selectedUnits = selectedUnits, selectedCell = option.loc }

                Action.Attack units toLoc ->
                    let
                        selectedUnits =
                            List.map (\unit -> unit.id) units

                        toCell =
                            Sort.Dict.get toLoc model.grid
                    in
                    case toCell of
                        Nothing ->
                            ( model, Cmd.none )

                        Just cell ->
                            update (MoveSelectedUnits cell) { model | selectedUnits = selectedUnits, selectedCell = option.loc }

                Action.BuildUnit unitType ->
                    update (BuildUnit unitType) { model | selectedCell = option.loc }

                Action.Research tech ->
                    update (ResearchTech tech) model

                Action.Upgrade upgradeType ->
                    update (UpgradeCell upgradeType) { model | selectedCell = option.loc }

                _ ->
                    ( model, Cmd.none )

        PerformAiTurn ->
            ( model, model |> Core.encoder |> Encode.encode 0 |> analyze )

        EndTurn ->
            let
                livingPlayers : List Player
                livingPlayers =
                    List.filter (\player -> not <| Game.Grid.townCountControlledBy model.grid player.id <= 0) model.players

                deadPlayers : List Player
                deadPlayers =
                    List.filter (\player -> Game.Grid.townCountControlledBy model.grid player.id <= 0) model.players

                nextActivePlayer : Player
                nextActivePlayer =
                    getNextActivePlayer livingPlayers livingPlayers model.activePlayer

                priests : List Unit
                priests =
                    List.filter (\unit -> unit.unitType == Game.UnitType.Priest) model.units

                priestLocs : List Loc
                priestLocs =
                    List.map (\unit -> unit.loc) priests

                capturedUnits : List Unit
                capturedUnits =
                    List.map (\unit -> captureUnit unit deadPlayers model.activePlayer) model.units

                healedUnits : List Unit
                healedUnits =
                    healUnits capturedUnits priestLocs

                capturedGrid : Grid
                capturedGrid =
                    model.grid
                        |> Sort.Dict.toList
                        |> List.map
                            (\( loc, cell ) ->
                                case cell.controlledBy of
                                    Nothing ->
                                        ( loc, cell )

                                    Just someone ->
                                        if List.foldl (\player thisPlayer -> thisPlayer || someone == player.id) False deadPlayers then
                                            ( loc, { cell | controlledBy = Just model.activePlayer, farms = 0, towers = 0 } )

                                        else
                                            ( loc, cell )
                            )
                        |> Sort.Dict.fromList Game.Grid.sorter

                newModel =
                    { model | activePlayer = nextActivePlayer.id, units = healedUnits, grid = capturedGrid }
            in
            update (StartTurn nextActivePlayer) newModel

        EndGame ->
            ( model, Core.delay 0 EndGame )

        OpenSettings ->
            ( model, Cmd.none )

        SaveGame ->
            ( model, model |> Core.encoder |> Encode.encode 0 |> saveGame )

        UpdateMobileTab tab ->
            ( { model | mobileTab = tab }, Cmd.none )

        ZoomIn ->
            ( model, zoomIn () )

        ZoomOut ->
            ( model, zoomOut () )

        NoOp ->
            ( model, Cmd.none )


runAiAction : Model -> Cmd Msg
runAiAction model =
    if (Game.Player.get model.players model.activePlayer).isHuman then
        Cmd.none

    else
        Core.delay model.aiSpeed PerformAiTurn


gainResources : PlayerStats -> Resources -> Resources
gainResources stats resources =
    let
        gold =
            resources.gold + stats.farms ()

        actions =
            let
                newActions =
                    Basics.toFloat (stats.towns ()) + resources.actions
            in
            3 + newActions
    in
    { gold = gold
    , actions = actions
    }


shouldCombatStart : List Unit -> List Int -> Bool
shouldCombatStart units playerIdList =
    let
        firstUnit =
            List.head units

        newPlayerIdList =
            case firstUnit of
                Nothing ->
                    playerIdList

                Just unit ->
                    if not (List.member unit.controlledBy playerIdList) then
                        playerIdList ++ [ unit.controlledBy ]

                    else
                        playerIdList
    in
    if List.length newPlayerIdList >= 2 then
        True

    else
        let
            remainingUnits =
                List.tail units
        in
        case remainingUnits of
            Nothing ->
                False

            Just newUnitsList ->
                shouldCombatStart newUnitsList newPlayerIdList


toCombat : Model -> ( Game.Combat.Model, Cmd Game.Combat.Msg ) -> ( Model, Cmd Msg )
toCombat model ( combat, cmd ) =
    ( { model | combat = Combat combat }, Cmd.map GotCombatMsg cmd )


toScenario : Model -> ( Game.Scenario.Model, Cmd Game.Scenario.Msg ) -> ( Model, Cmd Msg )
toScenario model ( scenario, cmd ) =
    ( { model | scenario = scenario }, Cmd.map GotScenarioMsg cmd )


getNextActivePlayer : List Player -> List Player -> Int -> Player
getNextActivePlayer initialList players playerId =
    case players of
        player :: remainingPlayers ->
            if player.id == playerId then
                case remainingPlayers of
                    nextPlayer :: _ ->
                        nextPlayer

                    [] ->
                        case List.head initialList of
                            Nothing ->
                                Game.Player.empty

                            Just aPlayer ->
                                aPlayer

            else
                getNextActivePlayer initialList remainingPlayers playerId

        [] ->
            case List.head initialList of
                Nothing ->
                    Game.Player.empty

                Just aPlayer ->
                    aPlayer


captureUnit : Unit -> List Player -> Int -> Unit
captureUnit unit deadPlayers playerId =
    case deadPlayers of
        dead :: otherDead ->
            if unit.controlledBy == dead.id then
                { unit | controlledBy = playerId }

            else
                captureUnit unit otherDead playerId

        [] ->
            unit


healUnits : List Unit -> List Loc -> List Unit
healUnits units inLocs =
    case inLocs of
        loc :: locs ->
            let
                healedUnits : List Unit
                healedUnits =
                    List.map
                        (\unit ->
                            if unit.loc == loc then
                                let
                                    maxHealth : Int
                                    maxHealth =
                                        unit.maxHealth
                                in
                                { unit
                                    | health =
                                        if unit.health + 1 > maxHealth then
                                            unit.health

                                        else
                                            unit.health + 1
                                }

                            else
                                unit
                        )
                        units
            in
            healUnits healedUnits locs

        [] ->
            units


recordAction : History -> Model -> Model
recordAction action model =
    let
        actionList : List History
        actionList =
            action :: model.actionHistory
    in
    { model | actionHistory = actionList }


port saveGame : String -> Cmd msg


port zoomIn : () -> Cmd msg


port zoomOut : () -> Cmd msg
