module Game.Scenario exposing (..)

import Game.Cell exposing (Cell)
import Game.Loc exposing (Loc)
import Game.Grid exposing (Grid)
import Game.Player exposing (NewPlayer, Player)
import Game.Structure
import Game.Unit exposing (Unit)
import Game.UnitType
import List.Extra as List
import Random
import Task


type ScenarioType
    = Conquest
    | ScoreReached Int
    | NumberOfTurns Int


type alias Model =
    { scenarioType : ScenarioType
    , maxX : Int
    , maxY : Int
    , currentX : Int
    , currentY : Int
    , finished : Bool
    , newPlayers : List NewPlayer
    , players : List Player
    , units : List Unit
    , grid : Grid
    , nextId : Int
    , activePlayerId : Int
    }


type alias Flags =
    { scenarioType : ScenarioType
    , maxX : Int
    , maxY : Int
    , players : List NewPlayer
    }


type Msg
    = InitializeScenario
    | GenerateNextCell Loc Int
    | RollNextCell Loc
    | GenerateNextPlayer (Maybe NewPlayer)
    | GenerateStartingLoc Player (List Player) Loc
    | RollStartingLocX Player (List Player)
    | RollStartingLocY Player (List Player) Int
    | MakeLocFromRolls Player (List Player) Int Int
    | DetermineFirstPlayer Int
    | ScenarioLoaded


init : Flags -> Model
init flags =
    { scenarioType = flags.scenarioType
    , maxX = flags.maxX
    , maxY = flags.maxY
    , currentX = 0
    , currentY = 0
    , finished = False
    , newPlayers = flags.players
    , players = []
    , units = []
    , grid = []
    , nextId = 1
    , activePlayerId = 0
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg scenario =
    case msg of
        InitializeScenario ->
            update (RollNextCell { x = 0, y = 0 }) scenario

        GenerateNextCell loc roll ->
            let
                nextX =
                    if scenario.currentY == scenario.maxY then
                        scenario.currentX + 1

                    else
                        scenario.currentX

                finished =
                    scenario.currentX > scenario.maxX

                nextY =
                    if scenario.currentY == scenario.maxY then
                        if finished == True then
                            scenario.currentY

                        else
                            0

                    else
                        scenario.currentY + 1

                newGrid =
                    if finished == True then
                        scenario.grid

                    else if scenario.currentY == 0 then
                        scenario.grid ++ [ [ Game.Cell.generate loc roll ] ]

                    else
                        List.map
                            (\row ->
                                if List.length row > scenario.maxY then
                                    row

                                else
                                    row ++ [ Game.Cell.generate loc roll ]
                            )
                            scenario.grid

                newScenario =
                    { scenario
                        | currentX = nextX
                        , currentY = nextY
                        , finished = finished
                        , grid = newGrid
                    }
            in
            update (RollNextCell { x = nextX, y = nextY }) newScenario

        RollNextCell loc ->
            if scenario.finished == False then
                ( scenario, Random.generate (GenerateNextCell loc) (randomDefinedMax 101) )

            else if List.length scenario.newPlayers > 0 then
                update (GenerateNextPlayer (List.head scenario.newPlayers)) scenario

            else
                ( scenario, Cmd.none )

        GenerateNextPlayer potentialNewPlayer ->
            case potentialNewPlayer of
                Nothing ->
                    ( scenario, Cmd.none )

                Just newPlayer ->
                    let
                        nextId =
                            scenario.nextId + 1

                        player =
                            Game.Player.generate newPlayer scenario.nextId

                        players =
                            scenario.players ++ [ player ]

                        remainingNewPlayers : List NewPlayer
                        remainingNewPlayers =
                            case List.tail scenario.newPlayers of
                                Nothing ->
                                    []

                                Just remainingNewPlayerstail ->
                                    remainingNewPlayerstail

                        newScenario =
                            { scenario
                                | newPlayers = remainingNewPlayers
                                , players = players
                                , nextId = nextId
                            }
                    in
                    if List.length remainingNewPlayers == 0 then
                        let
                            firstPlayer =
                                List.head newScenario.players

                            otherPlayers =
                                case List.tail newScenario.players of
                                    Nothing ->
                                        []

                                    Just otherPlayerList ->
                                        otherPlayerList
                        in
                        case firstPlayer of
                            Nothing ->
                                ( newScenario, Cmd.none )

                            Just aPlayer ->
                                update (RollStartingLocX aPlayer otherPlayers) newScenario

                    else
                        update (GenerateNextPlayer (List.head remainingNewPlayers)) newScenario

        GenerateStartingLoc player nextPlayers loc ->
            let
                cell : Maybe Cell
                cell =
                    Game.Cell.validStartingCell scenario.grid loc
            in
            case cell of
                Nothing ->
                    update (RollStartingLocX player nextPlayers) scenario

                Just realCell ->
                    if Game.Grid.distanceToEnemy scenario.grid { x = realCell.x, y = realCell.y } player.id <= 2 then
                        update (RollStartingLocX player nextPlayers) scenario

                    else
                        let
                            newGrid =
                                Game.Grid.replaceCell scenario.grid (Game.Cell.updateControl (Game.Cell.buildStructure realCell Game.Structure.Citadel) player.id)

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
                                [ Game.Unit.buildUnit Game.UnitType.Soldier player.id loc scenario.nextId, Game.Unit.buildUnit Game.UnitType.Soldier player.id loc (scenario.nextId + 1), Game.Unit.buildUnit Game.UnitType.Soldier player.id loc (scenario.nextId + 2) ]

                            nextscenario =
                                { scenario | grid = newGrid, units = scenario.units ++ newUnits, nextId = scenario.nextId + 3 }
                        in
                        case nextPlayer of
                            Nothing ->
                                ( nextscenario, Random.generate DetermineFirstPlayer (randomDefinedMax (List.length scenario.players)) )

                            Just theNextPlayer ->
                                update (RollStartingLocX theNextPlayer remainingPlayers) nextscenario

        RollStartingLocX player nextPlayers ->
            ( scenario, Random.generate (RollStartingLocY player nextPlayers) (randomDefinedMax (scenario.maxX + 1)) )

        RollStartingLocY player nextPlayers xVal ->
            ( scenario, Random.generate (MakeLocFromRolls player nextPlayers xVal) (randomDefinedMax (scenario.maxY + 1)) )

        MakeLocFromRolls player nextPlayers xVal yVal ->
            update (GenerateStartingLoc player nextPlayers { x = xVal, y = yVal }) scenario

        DetermineFirstPlayer roll ->
            let
                firstPlayer : Maybe Player
                firstPlayer =
                    List.getAt roll scenario.players
            in
            case firstPlayer of
                Nothing ->
                    ( scenario, Random.generate DetermineFirstPlayer (randomDefinedMax (List.length scenario.players)) )

                Just player ->
                    let
                        newScenario =
                            { scenario | activePlayerId = player.id }
                    in
                    ( newScenario, Task.succeed Cmd.none |> Task.perform (\_ -> ScenarioLoaded) )

        -- update ScenarioLoaded newScenario
        ScenarioLoaded ->
            ( scenario, Cmd.none )


randomDefinedMax : Int -> Random.Generator Int
randomDefinedMax max =
    Random.int 0 max
