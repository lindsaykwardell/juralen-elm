module Game exposing (..)

import Array
import Html exposing (Attribute, Html, br, button, div, span, table, td, text, tr, h3)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick, preventDefaultOn)
import Json.Decode as Json
import Juralen.Cell exposing (Cell, Loc)
import Juralen.CellType
import Juralen.Grid exposing (Grid)
import Juralen.Player exposing (NewPlayer, Player)
import Juralen.PlayerColor
import Juralen.Resources exposing (Resources)
import Juralen.Structure
import Juralen.Unit exposing (Unit)
import Juralen.UnitType exposing (UnitType)
import Random
import Game.Combat
import Game.Core exposing (..)
import Game.Analyzer exposing (analyze)
import Juralen.Analysis
import Juralen.Player
import Juralen.TechTree as TechTree exposing (TechTree, TechDescription, TechLevel(..))
import Game.Core
import Juralen.Resources
import Juralen.Resources
import Juralen.Analysis
import Juralen.UnitType exposing (InitialValues)
import Juralen.UnitType
import Juralen.Player
import Game.Core
import Juralen.Cell
import Juralen.Analysis
import Juralen.Analysis

init : List NewPlayer -> Float -> ( Model, Cmd Msg )
init newPlayerList aiSpeed =    
    update (RollNextCell { x = 0, y = 0 })
        { nextId = 1
        , grid = []
        , selectedCell = { x = 0, y = 0 }
        , players = []
        , activePlayer = 0
        , units = []
        , selectedUnits = []
        , init =
            { maxX = 8
            , maxY = 8
            , currentX = 0
            , currentY = 0
            , finished = False
            , newPlayers = newPlayerList
            }
        , combat = NoCombat
        , analysisResults = []
        , aiSpeed = aiSpeed
        }

randomDefinedMax : Int -> Random.Generator Int
randomDefinedMax max =
    Random.int 0 max


getNextActivePlayer : List Player -> List Player -> Int -> Player
getNextActivePlayer initialList players playerId =
    case players of
        (player :: remainingPlayers) ->
            if player.id == playerId then
                case remainingPlayers of
                    (nextPlayer :: _) ->
                        nextPlayer
                    
                    [] ->
                        case List.head initialList of 
                            Nothing ->
                                Juralen.Player.empty

                            Just aPlayer ->
                                aPlayer
            else getNextActivePlayer initialList remainingPlayers playerId
        [] ->
            case List.head initialList of 
                Nothing ->
                    Juralen.Player.empty

                Just aPlayer ->
                    aPlayer
            

gainResources : PlayerStats -> Resources -> Resources
gainResources stats resources =
    let
        gold =
            resources.gold + stats.farms

        actions =
            let
                newActions = Basics.toFloat stats.towns + resources.actions
            in
                3 + newActions
    in
    { gold = gold
    , actions = actions
    }


onContextMenu : msg -> Attribute msg
onContextMenu msg =
    preventDefaultOn "contextmenu" (Json.succeed ( msg, True ))

type Msg
    = GenerateNextCell Loc Int
    | RollNextCell Loc
    | GenerateNextPlayer (Maybe NewPlayer)
    | GenerateStartingLoc Player (List Player) Loc
    | RollStartingLocX Player (List Player)
    | RollStartingLocY Player (List Player) Int
    | MakeLocFromRolls Player (List Player) Int Int
    | DetermineFirstPlayer Int
    | StartTurn Player
    | Analyze
    | SelectCell Loc
    | BuildUnit UnitType
    | SelectUnit Int
    | MoveSelectedUnits Cell
    | ResearchTech TechDescription
    | UpgradeCell Juralen.Analysis.UpgradeType
    | GotCombatMsg Game.Combat.Msg
    | PerformAction Juralen.Analysis.Option
    | PerformAiTurn
    | EndTurn
    | EndGame


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GenerateNextCell loc roll ->
            let
                nextX =
                    if model.init.currentY == model.init.maxY then
                        model.init.currentX + 1

                    else
                        model.init.currentX

                finished =
                    model.init.currentX > model.init.maxX

                nextY =
                    if model.init.currentY == model.init.maxY then
                        if finished == True then
                            model.init.currentY

                        else
                            0

                    else
                        model.init.currentY + 1

                prevInit =
                    model.init

                nextInit =
                    { prevInit | currentX = nextX, currentY = nextY, finished = finished }

                newGrid =
                    if finished == True then
                        model.grid

                    else if model.init.currentY == 0 then
                        model.grid ++ [ [ Juralen.Cell.generate loc roll ] ]

                    else
                        List.map
                            (\row ->
                                if List.length row > model.init.maxY then
                                    row

                                else
                                    row ++ [ Juralen.Cell.generate loc roll ]
                            )
                            model.grid

                newModel =
                    { model | init = nextInit, grid = newGrid }
            in
            update (RollNextCell { x = nextX, y = nextY }) newModel

        RollNextCell loc ->
            if model.init.finished == False then
                ( model, Random.generate (GenerateNextCell loc) (randomDefinedMax 101) )

            else if List.length model.init.newPlayers > 0 then
                update (GenerateNextPlayer (List.head model.init.newPlayers)) model

            else
                ( model, Cmd.none )

        GenerateNextPlayer potentialNewPlayer ->
            case potentialNewPlayer of
                Nothing ->
                    ( model, Cmd.none )

                Just newPlayer ->
                    let
                        nextId =
                            model.nextId + 1

                        player =
                            Juralen.Player.generate newPlayer model.nextId

                        players =
                            model.players ++ [ player ]

                        remainingNewPlayers : List NewPlayer
                        remainingNewPlayers =
                            case List.tail model.init.newPlayers of
                                Nothing ->
                                    []

                                Just remainingNewPlayerstail ->
                                    remainingNewPlayerstail

                        prevInit =
                            model.init

                        newInit =
                            { prevInit | newPlayers = remainingNewPlayers }

                        newModel =
                            { model | players = players, init = newInit, nextId = nextId }
                    in
                    if List.length remainingNewPlayers == 0 then
                        let
                            firstPlayer =
                                List.head newModel.players

                            otherPlayers =
                                case List.tail newModel.players of
                                    Nothing ->
                                        []

                                    Just otherPlayerList ->
                                        otherPlayerList
                        in
                        case firstPlayer of
                            Nothing ->
                                ( newModel, Cmd.none )

                            Just aPlayer ->
                                update (RollStartingLocX aPlayer otherPlayers) newModel

                    else
                        update (GenerateNextPlayer (List.head remainingNewPlayers)) newModel

        GenerateStartingLoc player nextPlayers loc ->
            let
                cell : Maybe Cell
                cell =
                    Juralen.Cell.validStartingCell model.grid loc
            in
            case cell of
                Nothing ->
                    update (RollStartingLocX player nextPlayers) model

                Just realCell ->
                    if 
                        Juralen.Grid.distanceToEnemy model.grid { x = realCell.x, y = realCell.y } player.id <= 2
                    then 
                        update (RollStartingLocX player nextPlayers) model
                    else

                    let
                        newGrid =
                            Juralen.Grid.replaceCell model.grid (Juralen.Cell.updateControl (Juralen.Cell.buildStructure realCell Juralen.Structure.Citadel) player.id)

                        nextPlayer =
                            List.head nextPlayers

                        remainingPlayers =
                            case List.tail nextPlayers of
                                Nothing ->
                                    []

                                Just playerList ->
                                    playerList

                        newUnits : List Unit
                        newUnits =
                            [ Juralen.Unit.buildUnit Juralen.UnitType.Soldier player.id loc model.nextId, Juralen.Unit.buildUnit Juralen.UnitType.Soldier player.id loc (model.nextId + 1), Juralen.Unit.buildUnit Juralen.UnitType.Soldier player.id loc (model.nextId + 2) ]

                        nextModel =
                            { model | grid = newGrid, units = model.units ++ newUnits, nextId = model.nextId + 3 }
                    in
                    case nextPlayer of
                        Nothing ->
                            ( nextModel, Random.generate DetermineFirstPlayer (randomDefinedMax (List.length model.players)) )

                        Just theNextPlayer ->
                            update (RollStartingLocX theNextPlayer remainingPlayers) nextModel

        RollStartingLocX player nextPlayers ->
            ( model, Random.generate (RollStartingLocY player nextPlayers) (randomDefinedMax 9) )

        RollStartingLocY player nextPlayers xVal ->
            ( model, Random.generate (MakeLocFromRolls player nextPlayers xVal) (randomDefinedMax 9) )

        MakeLocFromRolls player nextPlayers xVal yVal ->
            update (GenerateStartingLoc player nextPlayers { x = xVal, y = yVal }) model

        DetermineFirstPlayer roll ->
            let
                firstPlayer : Maybe Player
                firstPlayer =
                    Array.get roll (Array.fromList model.players)
            in
            case firstPlayer of
                Nothing ->
                    ( model, Random.generate DetermineFirstPlayer (randomDefinedMax (List.length model.players)) )

                Just player ->
                    update (StartTurn player) model

        StartTurn nextActivePlayer ->
            if 
                Game.Core.getPlayerScore model nextActivePlayer.id >= 50 ||
                List.length (
                    List.filter (\player ->
                        not player.hasLost
                    )
                    model.players
                ) == 1
            then 
                update EndGame { model | activePlayer = -1}
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
                
                updatedUnits = List.map (\unit -> { unit | movesLeft = 
                    let
                        initialValues : Juralen.UnitType.InitialValues
                        initialValues = Juralen.UnitType.initialValues unit.unitType
                    in
                        initialValues.movesLeft }) model.units

                newModel = { model | activePlayer = nextActivePlayer.id, players = updatedPlayers, units = updatedUnits }
            in
            -- ( newModel, Cmd.none )
            update Analyze newModel

        Analyze ->            
            ( { model | analysisResults = analyze model}, 
                if (Juralen.Player.get model.players model.activePlayer).isHuman then
                    Cmd.none
                else
                    Game.Core.delay model.aiSpeed PerformAiTurn
            )

        SelectCell loc ->
            ( { model | selectedCell = loc, selectedUnits  = [] }, Cmd.none )

        BuildUnit unitType ->
            let
                newResources : Resources
                newResources =
                    Juralen.Resources.spend (Juralen.Player.getResources model.players model.activePlayer) (Juralen.UnitType.cost unitType)

                newUnit : Unit
                newUnit =
                    Juralen.Unit.buildUnit unitType model.activePlayer model.selectedCell model.nextId

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
            if (Juralen.Unit.fromId model.units unitId).controlledBy /= model.activePlayer || (Juralen.Unit.fromId model.units unitId).movesLeft <= 0 then
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
                        Juralen.Resources.useActions (Juralen.Player.getResources model.players model.activePlayer) (Basics.toFloat (Juralen.Cell.getDistance model.selectedCell { x = cell.x, y = cell.y }) * getMoveCost model)

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
                                    Juralen.Unit.isSelected model.selectedUnits unit.id
                                        && unit.x
                                        == model.selectedCell.x
                                        && unit.y
                                        == model.selectedCell.y
                                then
                                    { unit | x = cell.x, y = cell.y, movesLeft = unit.movesLeft - 1 }

                                else
                                    unit
                            )
                            model.units

                    newSelectedCell =
                        { x = cell.x, y = cell.y }

                    newCell =
                        if
                            cell.cellType
                                == Juralen.CellType.Plains
                                && List.length (Juralen.Unit.inCell model.units { x = cell.x, y = cell.y })
                                <= 0
                        then
                            Juralen.Cell.updateControl cell model.activePlayer

                        else
                            cell

                    newGrid =
                        Juralen.Grid.replaceCell model.grid newCell

                    newModel = { model | grid = newGrid, players = newPlayerList, units = newUnitList, selectedCell = newSelectedCell, selectedUnits = [] }
                in                    
                    if shouldCombatStart (Juralen.Unit.inCell newModel.units newModel.selectedCell) [] then
                        let
                            combatModel : Game.Combat.Model
                            combatModel = 
                                { units = Juralen.Unit.inCell newModel.units newModel.selectedCell
                                , deadUnits = []
                                , attacker = Juralen.Unit.empty
                                , defender = Juralen.Unit.empty
                                , attackingPlayer = Juralen.Player.get newModel.players newModel.activePlayer
                                , defendingPlayer = case Juralen.Cell.find newModel.grid newModel.selectedCell of
                                    Nothing ->
                                        Juralen.Player.empty

                                    Just selectedCell ->
                                        case selectedCell.controlledBy of
                                            Nothing ->
                                                Juralen.Player.empty

                                            Just defendingPlayerId ->
                                                Juralen.Player.get newModel.players defendingPlayerId
                                , whoGoesFirst = Game.Combat.Attacker
                                , defBonus = 0
                                }

                            startCombat : CombatStatus
                            startCombat = Combat combatModel
                                
                        in
                        update (GotCombatMsg (Game.Combat.GetRandomUnit Game.Combat.Attacker)) { newModel | combat = startCombat}

                    else
                        update Analyze newModel

        ResearchTech tech ->
            let
                _ = Debug.log  
                _ = Debug.log ("Researching! " ++ ((Just model.activePlayer) |> Juralen.Player.getName model.players)) tech.name
                stats = Game.Core.currentPlayerStats model

                cost = tech.cost
            in
                if stats.gold < cost then
                    (model, Cmd.none)

                else
                let
                    techTree = TechTree.research (Game.Core.getPlayerTechTree model.players model.activePlayer) tech.tech

                    players = List.map (\player -> if player.id == model.activePlayer then { player | techTree = techTree, resources = Juralen.Resources.spend player.resources tech.cost } else player) model.players
                in
                    update Analyze { model | players = players}
                
        UpgradeCell upgradeType ->
            let
                _ = Debug.log ("Upgrading! " ++ ((Just model.activePlayer) |> Juralen.Player.getName model.players)) upgradeType
                stats = Game.Core.currentPlayerStats model
            in

            case upgradeType of
                Juralen.Analysis.BuildFarm ->
                    if stats.gold < 2 then
                        (model, Cmd.none)

                    else
                        let
                            players = List.map (\player -> if player.id == model.activePlayer then { player | resources = Juralen.Resources.spend player.resources 2 } else player) model.players

                            cell : Cell
                            cell = Juralen.Cell.atLoc (Juralen.Grid.toList model.grid) model.selectedCell

                            grid : Grid
                            grid = Juralen.Grid.replaceCell model.grid { cell | farms = cell.farms + 1 }

                        in
                            update Analyze { model | players = players, grid = grid }

                Juralen.Analysis.BuildTower ->
                    if stats.gold < 2 then
                        (model, Cmd.none)

                    else
                        let
                            players = List.map (\player -> if player.id == model.activePlayer then { player | resources = Juralen.Resources.spend player.resources 2 } else player) model.players

                            cell : Cell
                            cell = Juralen.Cell.atLoc (Juralen.Grid.toList model.grid) model.selectedCell

                            grid : Grid
                            grid = Juralen.Grid.replaceCell model.grid { cell | towers = cell.towers + 1 }

                        in
                            update Analyze { model | players = players, grid = grid }

                Juralen.Analysis.RepairDefense ->
                    if stats.gold < 1 then
                        (model, Cmd.none)

                    else
                        let
                            players = List.map (\player -> if player.id == model.activePlayer then { player | resources = Juralen.Resources.spend player.resources 1 } else player) model.players

                            cell : Cell
                            cell = Juralen.Cell.atLoc (Juralen.Grid.toList model.grid) model.selectedCell

                            grid : Grid
                            grid = Juralen.Grid.replaceCell model.grid { cell | defBonus = if Juralen.Structure.initDef cell.structure > cell.defBonus then cell.defBonus + 1 else cell.defBonus }

                        in
                            update Analyze { model | players = players, grid = grid }

                        

        GotCombatMsg combatMsg ->
            case model.combat of
                NoCombat ->
                    ( model, Cmd.none)

                Combat combat ->
                    case combatMsg of
                        Game.Combat.ExitCombat ->
                            let
                                deadUnitIds = List.map (\unit -> unit.id) combat.deadUnits

                                newUnits : List Unit
                                newUnits = List.filter (\unit -> 
                                    not (List.member unit.id deadUnitIds)) (
                                        List.map (\unit -> 
                                            case List.head (
                                                List.filter (\livingUnit -> livingUnit.id == unit.id) combat.units) of
                                                
                                                Nothing ->
                                                    unit
                                                    
                                                Just livingUnit ->
                                                    livingUnit)
                                            
                                            model.units)
                                
                                winner = if List.length (Juralen.Unit.controlledBy combat.units combat.attackingPlayer.id) > 0 then combat.attackingPlayer else combat.defendingPlayer

                                updatedGrid : List (List Cell)
                                updatedGrid = List.map (\row -> 
                                    List.map (\cell -> 
                                        if cell.x == model.selectedCell.x && cell.y == model.selectedCell.y then 
                                            let
                                                conquered = cell.controlledBy /= Just winner.id
                                            in
                                                { cell 
                                                | controlledBy = Just winner.id
                                                , farms = if conquered then 0 else cell.farms
                                                , towers = if conquered then 0 else cell.towers
                                                , defBonus = if combat.defBonus < 0 then 0 else combat.defBonus
                                                } 
                                        else cell
                                    ) row) model.grid
                            in
                                update Analyze { model | units = newUnits, grid = updatedGrid}
                            

                        _ ->
                            toCombat model (Game.Combat.update combatMsg combat)

        PerformAction option ->
            case option.action of 
                Juralen.Analysis.Move units toLoc ->
                    let
                        selectedUnits = List.map (\unit -> unit.id) units

                        toCell = case Juralen.Cell.find model.grid toLoc of
                                    Nothing ->
                                        Juralen.Cell.empty

                                    Just cell ->
                                        cell
                    in
                        update (MoveSelectedUnits toCell) { model | selectedUnits = selectedUnits, selectedCell = option.loc}

                Juralen.Analysis.Attack units toLoc ->
                    let
                        selectedUnits = List.map (\unit -> unit.id) units

                        toCell = case Juralen.Cell.find model.grid toLoc of
                                    Nothing ->
                                        Juralen.Cell.empty

                                    Just cell ->
                                        cell
                    in
                        update (MoveSelectedUnits toCell) { model | selectedUnits = selectedUnits, selectedCell = option.loc}

                Juralen.Analysis.BuildUnit unitType ->
                    update (BuildUnit unitType) { model | selectedCell = option.loc}  

                Juralen.Analysis.Research tech ->
                    update (ResearchTech tech) model

                Juralen.Analysis.Upgrade upgradeType ->
                    update (UpgradeCell upgradeType) { model | selectedCell = option.loc }

                _ ->
                    (model, Cmd.none)

        PerformAiTurn ->
            case List.head (model.analysisResults) of
                Nothing ->
                    update (EndTurn) model

                Just option ->
                    update (PerformAction option) model

        EndTurn ->
            let
                players : List Player
                players = List.map (\player -> 
                    let
                        score = getPlayerScore model player.id    

                        hasLost = Juralen.Grid.townCountControlledBy model.grid player.id <= 0 
                    in
                        { player | hasLost = hasLost, score = score }
                    ) model.players

                livingPlayers : List Player
                livingPlayers = (List.filter (\player -> not player.hasLost) players)

                deadPlayers : List Player
                deadPlayers = List.filter (\player -> player.hasLost) players

                nextActivePlayer : Player
                nextActivePlayer =
                    getNextActivePlayer livingPlayers livingPlayers model.activePlayer

                priests : List Unit
                priests = List.filter (\unit -> unit.unitType == Juralen.UnitType.Priest) model.units

                priestLocs : List Loc
                priestLocs = List.map (\unit -> { x = unit.x, y = unit.y }) priests

                capturedUnits : List Unit
                capturedUnits = List.map (\unit -> captureUnit unit deadPlayers model.activePlayer) model.units

                healedUnits : List Unit
                healedUnits = healUnits capturedUnits priestLocs

                capturedGrid : List (List Cell)
                capturedGrid = 
                    List.map (\row -> 
                        List.map (\cell -> 
                            case cell.controlledBy of
                                Nothing ->
                                    cell

                                Just someone ->
                                    if List.foldl (\player thisPlayer -> thisPlayer || someone == player.id) False deadPlayers then
                                        { cell | controlledBy = Just model.activePlayer, farms = 0, towers = 0 }

                                    else
                                        cell
                            ) 
                        row) 
                    model.grid

                newModel = { model | activePlayer = nextActivePlayer.id, players = players, units = healedUnits, grid = capturedGrid }
            in
            update (StartTurn nextActivePlayer) newModel
        
        EndGame ->
            ( model, Game.Core.delay 0 EndGame )

captureUnit : Unit -> List Player -> Int -> Unit
captureUnit unit deadPlayers playerId =
    case deadPlayers of
        (dead :: otherDead) ->
            if unit.controlledBy == dead.id then
                { unit | controlledBy = playerId }

            else
                captureUnit unit otherDead playerId

        [] ->
            unit

healUnits : List Unit -> List Loc -> List Unit
healUnits units inLocs =
    case inLocs of
        (loc :: locs) ->
            let
                healedUnits : List Unit
                healedUnits = List.map (\unit -> 
                    if unit.x == loc.x && unit.y == loc.y then
                        let
                            initialValues : InitialValues
                            initialValues = Juralen.UnitType.initialValues unit.unitType

                            maxHealth : Int
                            maxHealth = initialValues.health
                        in
                            { unit | health = if unit.health + 1 > maxHealth then unit.health else unit.health + 1 }

                    else unit) units
            in
                healUnits healedUnits locs
            
        [] ->
            units

toCombat : Model -> (Game.Combat.Model, Cmd Game.Combat.Msg) -> ( Model, Cmd Msg)
toCombat model (combat, cmd) =
    ( { model | combat = Combat combat }, Cmd.map GotCombatMsg cmd)

shouldCombatStart : List Unit -> List Int -> Bool
shouldCombatStart units playerIdList =
    let
        firstUnit = List.head units

        remainingUnits = List.tail units

        newPlayerIdList = 
            case firstUnit of
                Nothing ->
                    playerIdList

                Just unit ->
                    if not (List.member unit.controlledBy playerIdList) then
                        playerIdList ++ [unit.controlledBy]

                    else
                        playerIdList

    in
        if List.length newPlayerIdList>= 2 then True
        else
            case remainingUnits of
                Nothing ->
                    False

                Just newUnitsList ->
                    shouldCombatStart newUnitsList newPlayerIdList
    

view : Model -> Html Msg
view model = 
    div [ class "flex" ]
            [ div [ class "w-3/5 p-3" ]
                [
                    table [ class "w-full" ]
                    (List.map
                    (\row ->
                        tr []
                            (List.map
                                (\cell ->
                                    td []
                                        [ div
                                            [ class ("cell " ++ Juralen.Cell.getColorClass cell model.players)
                                            , style "border"
                                                (if cell.x == model.selectedCell.x && cell.y == model.selectedCell.y then
                                                    "2px solid yellow"

                                                 else
                                                    ""
                                                )
                                            , onClick (SelectCell { x = cell.x, y = cell.y })
                                            , onContextMenu (MoveSelectedUnits cell)
                                            ]
                                            [ text
                                                (if isInRange model cell then
                                                    "## " ++ Juralen.CellType.toString cell.cellType ++ " ##"

                                                 else
                                                    Juralen.CellType.toString cell.cellType
                                                )
                                            , br [] []
                                            , text (Juralen.Structure.toString cell.structure)
                                            , br [] []
                                            , div []
                                                (List.map
                                                    (\unit ->
                                                        span [ class "unit" ]
                                                            [ if Juralen.Unit.isSelected model.selectedUnits unit.id then
                                                                span []
                                                                    [ text "[ "
                                                                    , text (Juralen.UnitType.short unit.unitType)
                                                                    , text " ]"
                                                                    ]

                                                              else
                                                                span [] [ text (Juralen.UnitType.short unit.unitType) ]
                                                            ]
                                                    )
                                                    (Juralen.Unit.inCell model.units { x = cell.x, y = cell.y })
                                                )
                                            ]
                                        ]
                                )
                                row
                            )
                    )
                    model.grid
                )
                , div [ class "p-3 w-64" ] (List.map (\player -> div [ class (
                        Juralen.PlayerColor.toClass player.color
                        ++ (if Juralen.PlayerColor.isDark player.color then " text-white" else " text-black")
                    ) ] 
                    [ text (player.name ++ " - " ++ String.fromInt player.score) ]) (List.sortBy .score model.players |> List.reverse))]
                
            , div [ class "w-2/5 p-3" ]
                [ if model.activePlayer /= -1 then div [ class "p-3" ]
                    [ button [ class "py-2 w-full bg-green-500 hover:bg-green-400", onClick EndTurn ] [ text "End Turn" ] 
                    ] else text ""
                , if model.activePlayer /= -1 then div [ class ("text-center p-3 " ++ Juralen.Player.getColorClass model.players (Just model.activePlayer)) ]
                    [ text (Juralen.Player.getName model.players (Just model.activePlayer))
                    , div [ class "flex" ]
                        [ div [ class "flex-1 p-2" ] [ text "Gold: ", text (String.fromInt (currentPlayerStats model).gold) ]
                        , div [ class "flex-1 p-2" ] [ text "Actions: ", text (String.fromFloat (currentPlayerStats model).actions) ]
                        , div [ class "flex-1 p-2" ] [ text "Farms: ", text (String.fromInt (currentPlayerStats model).farms) ]
                        , div [ class "flex-1 p-2" ] [ text "Towns: ", text (String.fromInt (currentPlayerStats model).towns) ]
                        , div [ class "flex-1 p-2" ] [ text "Units: ", text (String.fromInt (currentPlayerStats model).units) ]
                        ]
                    ] else text ""
                , div [ class "mt-4 border-2 rounded" ]
                    [ div
                        [ class
                            ("p-3 "
                                ++ (case Juralen.Cell.find model.grid model.selectedCell of
                                        Nothing ->
                                            ""

                                        Just selectedCell ->
                                            Juralen.Cell.getColorClass selectedCell model.players
                                   )
                            )
                        ]
                        [ text (String.fromInt model.selectedCell.x)
                        , text ", "
                        , text (String.fromInt model.selectedCell.y)
                        , br [] []
                        , div [ class "flex" ]
                            [ div [ class "flex-1" ]
                                [ text
                                    (case Juralen.Cell.find model.grid model.selectedCell of
                                        Nothing ->
                                            ""

                                        Just selectedCell ->
                                            Juralen.CellType.toString selectedCell.cellType ++
                                                case selectedCell.structure of
                                                    Nothing ->
                                                        ""
                                                    
                                                    Just structure ->
                                                        " [" ++ Juralen.Structure.toString selectedCell.structure ++ "]"
                                    )
                                ]
                            , div [ class "flex-1 italic" ]
                                [ text
                                    (case Juralen.Cell.find model.grid model.selectedCell of
                                        Nothing ->
                                            "Not Controlled"

                                        Just selectedCell ->
                                            "("
                                                ++ (case selectedCell.controlledBy of
                                                        Nothing ->
                                                            "Not Controlled"

                                                        _ ->
                                                            Juralen.Player.getName model.players selectedCell.controlledBy
                                                   )
                                                ++ ")"
                                    )
                                ]
                            ]
                        , div [ class "flex" ]
                            [ div [ class "flex-1" ] 
                                [ text ("Defense Bonus: " ++ String.fromInt (Juralen.Cell.atLoc (Juralen.Grid.toList model.grid) model.selectedCell |> .defBonus))  ]
                            , div [ class "flex-1" ] 
                                [ text ("Farms: " ++ String.fromInt (Juralen.Cell.atLoc (Juralen.Grid.toList model.grid) model.selectedCell |> .farms))  ]
                            , div [ class "flex-1" ] 
                                [ text ("Towers: " ++ String.fromInt (Juralen.Cell.atLoc (Juralen.Grid.toList model.grid) model.selectedCell |> .towers))  ]
                            ]
                        ]
                    ]
                , div []
                    (List.map (\buildableUnit -> button [ class "bg-blue-400 hover:bg-blue-200 py-2 px-3 rounded m-2", onClick (BuildUnit buildableUnit) ] [ text ("Build " ++ Juralen.UnitType.toString buildableUnit) ])
                        (case Juralen.Cell.find model.grid model.selectedCell of
                            Nothing ->
                                []

                            Just selectedCell ->
                                case selectedCell.controlledBy of
                                    Nothing ->
                                        []

                                    Just controlledBy ->
                                        if controlledBy /= model.activePlayer then
                                            []

                                        else
                                            Juralen.Structure.canBuild selectedCell.structure (currentPlayerStats model |> .techTree)
                        )
                    )
                , div []
                    (Game.Core.getPlayerTechTree model.players model.activePlayer |> TechTree.nextAvailableTech |> List.map techTreeButton)
                , div [ class "p-5" ]
                    (if (Juralen.Cell.atLoc (Juralen.Grid.toList model.grid) model.selectedCell |> .controlledBy) /= Just model.activePlayer then [] else
                    [ if 
                        ((Juralen.Cell.atLoc (Juralen.Grid.toList model.grid) model.selectedCell) |> .defBonus) 
                        < 
                        Juralen.Structure.initDef ((Juralen.Cell.atLoc (Juralen.Grid.toList model.grid) model.selectedCell) |> .structure) 
                        then 
                            button [ class "bg-green-400 hover:bg-green-200 py-2 px-3 rounded m-2", onClick (UpgradeCell Juralen.Analysis.RepairDefense)] [ text "Repair Defense (1)"]
                        else text ""
                    , case getPlayerTechTree model.players model.activePlayer |> .levelOne of
                        Nothing ->
                            text ""

                        Just tech ->
                            let
                                buttonText = if tech == TechTree.BuildFarms then "Build Farm (2)" else "Build Tower (2)"

                                action = if tech == TechTree.BuildFarms then Juralen.Analysis.BuildFarm else Juralen.Analysis.BuildTower
                            in
                                button [ class "bg-green-400 hover:bg-green-200 py-2 px-3 rounded m-2", onClick (UpgradeCell action)] [ text buttonText]
                    ])
                , div [ class "p-5" ]
                    (List.map
                        (\unit ->
                            div
                                [ class
                                    ("flex p-2 my-2 rounded text-white pointer"
                                        ++ (if Juralen.Unit.isSelected model.selectedUnits unit.id then
                                                " bg-blue-700 hover:bg-blue-600"

                                            else
                                                " bg-gray-700 hover:bg-gray-600"
                                           )
                                    )
                                , onClick (SelectUnit unit.id)
                                ]
                                [ div [ class "flex flex-col mr-2" ]
                                    [ div [ class ("triangle " ++ Juralen.PlayerColor.toString (Juralen.Player.getColor model.players unit.controlledBy)) ] []
                                    , div [ class ("triangle " ++ Juralen.PlayerColor.toString (Juralen.Player.getColor model.players unit.controlledBy)) ] []
                                    ]
                                , div [ class "w-1/3 text-left" ] [ text (Juralen.UnitType.toString unit.unitType) ]
                                , div [ class "flex-1" ] [ text "Atk: ", text (String.fromInt unit.attack) ]
                                , div [ class "flex-1" ] [ text "HP: ", text (String.fromInt unit.health) ]
                                , div [ class "flex-1" ] [ text "Moves: ", text (String.fromInt unit.movesLeft) ]
                                ]
                        )
                        (Juralen.Unit.inCell model.units model.selectedCell)
                    )
                , div [ class "p-5" ]
                    [ h3 [ class "text-white" ] [ text "Recommended Actions"]]
                    , div [ ] (List.map
                        (\option ->
                            div 
                                [ class "flex p-2 my-2 rounded text-white pointe text-white p-2 bg-gray-700 hover:bg-gray-600 pointer"
                                  , onClick (PerformAction option) 
                                ] 
                            [ div [class "flex-grow"] [text (Juralen.Analysis.toString option)]
                            , div [class "flex-shrink"] [ text (String.fromInt option.score)]])
                        (List.take 5 model.analysisResults))
                ]
            ]

techTreeButton : TechDescription -> Html Msg
techTreeButton tech =
    button [ class "bg-yellow-400 hover:bg-yellow-200 py-2 px-3 rounded m-2"
           , onClick (ResearchTech tech)
           ] 
        [ text (tech.name ++ " (" ++ String.fromInt tech.cost ++ ")")
        , br [] []
        , text tech.description ]