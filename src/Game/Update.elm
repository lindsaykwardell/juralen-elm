module Game.Update exposing (Msg(..), update)

import Game.Analysis
import Game.Analyzer exposing (analyze)
import Game.Cell exposing (Cell)
import Game.CellType
import Game.Combat
import Game.Core exposing (..)
import Game.Grid exposing (Grid)
import Game.Loc as Loc exposing (Loc)
import Game.Player exposing (Player)
import Game.Resources exposing (Resources)
import Game.Scenario exposing (Msg(..))
import Game.Structure
import Game.TechTree as TechTree exposing (TechDescription, TechLevel(..))
import Game.Unit exposing (Unit)
import Game.UnitType exposing (InitialValues, UnitType)


type Msg
    = InitializeScenario
    | GotScenarioMsg Game.Scenario.Msg
    | StartTurn Player
    | Analyze
    | SelectCell Loc
    | BuildUnit UnitType
    | SelectUnit Int
    | MoveSelectedUnits Cell
    | ResearchTech TechDescription
    | UpgradeCell Game.Analysis.UpgradeType
    | GotCombatMsg Game.Combat.Msg
    | PerformAction Game.Analysis.Option
    | PerformAiTurn
    | EndTurn
    | EndGame
    | SaveGame
    | LoadGame
    | OpenSettings
    | UpdateMobileTab MobileTab


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
                ScenarioLoaded ->
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
                List.length
                    (List.filter
                        (\player ->
                            not player.hasLost
                        )
                        model.players
                    )
                    == 1
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
                                    | movesLeft =
                                        let
                                            initialValues : Game.UnitType.InitialValues
                                            initialValues =
                                                Game.UnitType.initialValues unit.unitType
                                        in
                                        initialValues.movesLeft
                                }
                            )
                            model.units

                    newModel =
                        { model
                            | turn = model.turn + 1
                            , activePlayer = nextActivePlayer.id
                            , players = updatedPlayers
                            , units = updatedUnits
                        }
                in
                -- ( newModel, Cmd.none )
                update Analyze newModel

        Analyze ->
            ( { model | analysisResults = analyze model }
            , if (Game.Player.get model.players model.activePlayer).isHuman then
                Cmd.none

              else
                Game.Core.delay model.aiSpeed PerformAiTurn
            )

        SelectCell loc ->
            ( { model | selectedCell = loc, selectedUnits = [] }, Cmd.none )

        BuildUnit unitType ->
            let
                newResources : Resources
                newResources =
                    Game.Resources.spend (Game.Player.get model.players model.activePlayer |> .resources) (Game.UnitType.cost unitType)

                newUnit : Unit
                newUnit =
                    Game.Unit.buildUnit unitType model.activePlayer model.selectedCell model.nextId

                nextId : Int
                nextId =
                    model.nextId + 1

                newUnitList =
                    model.units ++ [ newUnit ]

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
                update Analyze { model | nextId = nextId, units = newUnitList, players = newPlayerList }

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
            if isInRange model cell == False then
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

                    newModel =
                        { model | grid = newGrid, players = newPlayerList, units = newUnitList, selectedCell = newSelectedCell, selectedUnits = [] }
                in
                if shouldCombatStart (Game.Unit.inCell newModel.units newModel.selectedCell) [] then
                    let
                        combatModel : Game.Combat.Model
                        combatModel =
                            { units = Game.Unit.inCell newModel.units newModel.selectedCell
                            , deadUnits = []
                            , attacker = Game.Unit.empty
                            , defender = Game.Unit.empty
                            , attackingPlayer = Game.Player.get newModel.players newModel.activePlayer
                            , defendingPlayer =
                                case Game.Cell.find newModel.grid newModel.selectedCell of
                                    Nothing ->
                                        Game.Player.empty

                                    Just selectedCell ->
                                        case selectedCell.controlledBy of
                                            Nothing ->
                                                Game.Player.empty

                                            Just defendingPlayerId ->
                                                Game.Player.get newModel.players defendingPlayerId
                            , whoGoesFirst = Game.Combat.Attacker
                            , defBonus = 0
                            }

                        startCombat : CombatStatus
                        startCombat =
                            Combat combatModel
                    in
                    update (GotCombatMsg (Game.Combat.GetRandomUnit Game.Combat.Attacker)) { newModel | combat = startCombat }

                else
                    update Analyze newModel

        ResearchTech tech ->
            let
                stats =
                    Game.Core.currentPlayerStats model

                cost =
                    tech.cost
            in
            if stats.gold < cost then
                ( model, Cmd.none )

            else
                let
                    techTree =
                        TechTree.research (Game.Core.getPlayerTechTree model.players model.activePlayer) tech.tech

                    players =
                        List.map
                            (\player ->
                                if player.id == model.activePlayer then
                                    { player | techTree = techTree, resources = Game.Resources.spend player.resources tech.cost }

                                else
                                    player
                            )
                            model.players
                in
                update Analyze { model | players = players }

        UpgradeCell upgradeType ->
            let
                stats =
                    Game.Core.currentPlayerStats model
            in
            case upgradeType of
                Game.Analysis.BuildFarm ->
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
                                Game.Cell.find model.grid model.selectedCell

                            grid : Grid
                            grid =
                                case cell of
                                    Nothing ->
                                        model.grid

                                    Just aCell ->
                                        Game.Grid.replaceCell model.grid { aCell | farms = aCell.farms + 1 }
                        in
                        update Analyze { model | players = players, grid = grid }

                Game.Analysis.BuildTower ->
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
                                Game.Cell.find model.grid model.selectedCell

                            grid : Grid
                            grid =
                                case cell of
                                    Nothing ->
                                        model.grid

                                    Just aCell ->
                                        Game.Grid.replaceCell model.grid { aCell | towers = aCell.towers + 1 }
                        in
                        update Analyze { model | players = players, grid = grid }

                Game.Analysis.RepairDefense ->
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
                                Game.Cell.find model.grid model.selectedCell

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
                        in
                        update Analyze { model | players = players, grid = grid }

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

                                updatedGrid : List (List Cell)
                                updatedGrid =
                                    List.map
                                        (\row ->
                                            List.map
                                                (\cell ->
                                                    if cell.loc == model.selectedCell then
                                                        let
                                                            conquered =
                                                                cell.controlledBy /= Just winner.id
                                                        in
                                                        { cell
                                                            | controlledBy = Just winner.id
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

                                                    else
                                                        cell
                                                )
                                                row
                                        )
                                        model.grid
                            in
                            update Analyze { model | units = newUnits, grid = updatedGrid }

                        _ ->
                            toCombat model (Game.Combat.update combatMsg combat)

        PerformAction option ->
            case option.action of
                Game.Analysis.Move units toLoc ->
                    let
                        selectedUnits =
                            List.map (\unit -> unit.id) units

                        toCell =
                            Game.Cell.find model.grid toLoc
                    in
                    case toCell of
                        Nothing ->
                            ( model, Cmd.none )

                        Just cell ->
                            update (MoveSelectedUnits cell) { model | selectedUnits = selectedUnits, selectedCell = option.loc }

                Game.Analysis.Attack units toLoc ->
                    let
                        selectedUnits =
                            List.map (\unit -> unit.id) units

                        toCell =
                            Game.Cell.find model.grid toLoc
                    in
                    case toCell of
                        Nothing ->
                            ( model, Cmd.none )

                        Just cell ->
                            update (MoveSelectedUnits cell) { model | selectedUnits = selectedUnits, selectedCell = option.loc }

                Game.Analysis.BuildUnit unitType ->
                    update (BuildUnit unitType) { model | selectedCell = option.loc }

                Game.Analysis.Research tech ->
                    update (ResearchTech tech) model

                Game.Analysis.Upgrade upgradeType ->
                    update (UpgradeCell upgradeType) { model | selectedCell = option.loc }

                _ ->
                    ( model, Cmd.none )

        PerformAiTurn ->
            case List.head model.analysisResults of
                Nothing ->
                    update EndTurn model

                Just option ->
                    update (PerformAction option) model

        EndTurn ->
            let
                players : List Player
                players =
                    List.map
                        (\player ->
                            let
                                score =
                                    getPlayerScore model player.id

                                hasLost =
                                    Game.Grid.townCountControlledBy model.grid player.id <= 0
                            in
                            { player | hasLost = hasLost, score = score }
                        )
                        model.players

                livingPlayers : List Player
                livingPlayers =
                    List.filter (\player -> not player.hasLost) players

                deadPlayers : List Player
                deadPlayers =
                    List.filter (\player -> player.hasLost) players

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

                capturedGrid : List (List Cell)
                capturedGrid =
                    List.map
                        (\row ->
                            List.map
                                (\cell ->
                                    case cell.controlledBy of
                                        Nothing ->
                                            cell

                                        Just someone ->
                                            if List.foldl (\player thisPlayer -> thisPlayer || someone == player.id) False deadPlayers then
                                                { cell | controlledBy = Just model.activePlayer, farms = 0, towers = 0 }

                                            else
                                                cell
                                )
                                row
                        )
                        model.grid

                newModel =
                    { model | activePlayer = nextActivePlayer.id, players = players, units = healedUnits, grid = capturedGrid }
            in
            update (StartTurn nextActivePlayer) newModel

        EndGame ->
            ( model, Game.Core.delay 0 EndGame )

        OpenSettings ->
            ( model, Cmd.none )

        SaveGame ->
            ( model, Cmd.none )

        LoadGame ->
            ( model, Cmd.none )

        UpdateMobileTab tab ->
            ( { model | mobileTab = tab }, Cmd.none )


gainResources : PlayerStats -> Resources -> Resources
gainResources stats resources =
    let
        gold =
            resources.gold + stats.farms

        actions =
            let
                newActions =
                    Basics.toFloat stats.towns + resources.actions
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

        remainingUnits =
            List.tail units

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
                                    initialValues : InitialValues
                                    initialValues =
                                        Game.UnitType.initialValues unit.unitType

                                    maxHealth : Int
                                    maxHealth =
                                        initialValues.health
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
