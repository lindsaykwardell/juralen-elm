module Game.Scenario exposing (Flags, Model, Msg(..), ScenarioType(..), decoder, encoder, init, isEndConditionReached, onSelectScenario, scenarioTypeDecoder, scenarioTypeEncoder, update)

import Dict
import Game.Cell exposing (Cell)
import Game.Grid exposing (Grid)
import Game.Loc as Loc exposing (Loc)
import Game.NewPlayer
import Game.Player exposing (NewPlayer, Player)
import Game.Structure
import Game.Unit exposing (Unit)
import Game.UnitType
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode
import Json.Encode as Encode
import List.Extra as List
import Random
import Sort.Dict
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
    , playerCount : Int
    , newPlayers : List NewPlayer
    , players : List Player
    , units : List Unit
    , grid : Grid
    , nextId : Int
    , activePlayerId : Int
    }


decoder : Decoder Model
decoder =
    Decode.succeed Model
        |> Decode.required "scenarioType" scenarioTypeDecoder
        |> Decode.required "maxX" Decode.int
        |> Decode.required "maxY" Decode.int
        |> Decode.required "currentX" Decode.int
        |> Decode.required "currentY" Decode.int
        |> Decode.required "finished" Decode.bool
        |> Decode.required "playerCount" Decode.int
        |> Decode.required "newPlayers" (Decode.list Game.NewPlayer.decoder)
        |> Decode.required "players" (Decode.list Game.Player.decoder)
        |> Decode.required "units" (Decode.list Game.Unit.decoder)
        |> Decode.required "grid" Game.Grid.decoder
        |> Decode.required "nextId" Decode.int
        |> Decode.required "activePlayerId" Decode.int


encoder : Model -> Encode.Value
encoder model =
    Encode.object
        [ ( "scenarioType"
          , scenarioTypeEncoder model.scenarioType
          )
        , ( "maxX", Encode.int model.maxX )
        , ( "maxY", Encode.int model.maxY )
        , ( "currentX", Encode.int model.currentX )
        , ( "currentY", Encode.int model.currentY )
        , ( "finished", Encode.bool model.finished )
        , ( "playerCount", Encode.int model.playerCount )
        , ( "newPlayers", Encode.list Game.NewPlayer.encoder model.newPlayers )
        , ( "players", Encode.list Game.Player.encoder model.players )
        , ( "units", Encode.list Game.Unit.encoder model.units )
        , ( "grid", Game.Grid.encoder model.grid )
        , ( "nextId", Encode.int model.nextId )
        , ( "activePlayerId", Encode.int model.activePlayerId )
        ]


scenarioTypeDecoder : Decoder ScenarioType
scenarioTypeDecoder =
    Decode.string
        |> Decode.dict
        |> Decode.andThen
            (\t ->
                case Dict.get "scenarioName" t of
                    Nothing ->
                        Decode.fail "scenarioName not found"

                    Just name ->
                        case name of
                            "conquest" ->
                                Decode.succeed Conquest

                            "scoreReached" ->
                                case Dict.get "score" t of
                                    Nothing ->
                                        Decode.fail "score not found"

                                    Just strScore ->
                                        case String.toInt strScore of
                                            Nothing ->
                                                Decode.fail "score not an integer"

                                            Just score ->
                                                Decode.succeed (ScoreReached score)

                            "numberOfTurns" ->
                                case Dict.get "turns" t of
                                    Nothing ->
                                        Decode.fail "turns not found"

                                    Just strTurns ->
                                        case String.toInt strTurns of
                                            Nothing ->
                                                Decode.fail "turns not an integer"

                                            Just turns ->
                                                Decode.succeed (NumberOfTurns turns)

                            _ ->
                                Decode.fail "unknown scenarioName"
            )


scenarioTypeEncoder : ScenarioType -> Encode.Value
scenarioTypeEncoder scenarioType =
    Encode.object
        (case scenarioType of
            Conquest ->
                [ ( "scenarioName", Encode.string "conquest" ) ]

            ScoreReached score ->
                [ ( "scenarioName", Encode.string "scoreReached" ), ( "score", Encode.string (String.fromInt score) ) ]

            NumberOfTurns turns ->
                [ ( "scenarioName", Encode.string "numberOfTurns" ), ( "turns", Encode.string (String.fromInt turns) ) ]
        )


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
    , playerCount = List.length flags.players
    , newPlayers = flags.players
    , players = []
    , units = []
    , grid = Sort.Dict.empty Game.Grid.sorter
    , nextId = 1
    , activePlayerId = 0
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg scenario =
    case msg of
        InitializeScenario ->
            update (RollNextCell <| Loc.at 0 0) scenario

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
                        if finished then
                            scenario.currentY

                        else
                            0

                    else
                        scenario.currentY + 1

                newGrid =
                    if finished then
                        scenario.grid

                    else
                        Sort.Dict.insert loc (Game.Cell.generate loc roll) scenario.grid

                newScenario =
                    { scenario
                        | currentX = nextX
                        , currentY = nextY
                        , finished = finished
                        , grid = newGrid
                    }
            in
            update (RollNextCell <| Loc.at nextX nextY) newScenario

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
                    Game.Grid.validStartingCell scenario.grid loc
            in
            case cell of
                Nothing ->
                    update (RollStartingLocX player nextPlayers) scenario

                Just realCell ->
                    let
                        minDistanceBetweenPlayers =
                            0

                        --scenario.maxX * scenario.maxY // scenario.playerCount // 3
                    in
                    if Game.Grid.distanceToEnemy scenario.grid realCell.loc player.id <= minDistanceBetweenPlayers then
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
                                ( nextscenario, Random.generate DetermineFirstPlayer (randomDefinedMax (List.length scenario.players - 1)) )

                            Just theNextPlayer ->
                                update (RollStartingLocX theNextPlayer remainingPlayers) nextscenario

        RollStartingLocX player nextPlayers ->
            ( scenario, Random.generate (RollStartingLocY player nextPlayers) (randomDefinedMax (scenario.maxX + 1)) )

        RollStartingLocY player nextPlayers xVal ->
            ( scenario, Random.generate (MakeLocFromRolls player nextPlayers xVal) (randomDefinedMax (scenario.maxY + 1)) )

        MakeLocFromRolls player nextPlayers xVal yVal ->
            update (GenerateStartingLoc player nextPlayers <| Loc.at xVal yVal) scenario

        DetermineFirstPlayer roll ->
            let
                firstPlayer : Maybe Player
                firstPlayer =
                    List.getAt roll scenario.players
            in
            case firstPlayer of
                Nothing ->
                    ( scenario, Random.generate DetermineFirstPlayer (randomDefinedMax (List.length scenario.players - 1)) )

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


isEndConditionReached :
    { players : List Player
    , nextActivePlayer : Player
    , units : List Unit
    , getPlayerScore : Int -> Int
    , turn : Int
    , townCountControlledBy : Int -> Int
    }
    -> Model
    -> Bool
isEndConditionReached config model =
    let
        onePlayerLeft =
            List.length
                (List.filter
                    (\player ->
                        not <| config.townCountControlledBy player.id <= 0
                    )
                    config.players
                )
                == 1
    in
    case model.scenarioType of
        Conquest ->
            onePlayerLeft

        ScoreReached winningScore ->
            onePlayerLeft
                || config.getPlayerScore config.nextActivePlayer.id
                >= winningScore

        NumberOfTurns numberOfTurns ->
            onePlayerLeft || config.turn >= numberOfTurns * List.length config.players


onSelectScenario : String -> ScenarioType
onSelectScenario scenarioName =
    case scenarioName of
        "CONQUEST" ->
            Conquest

        "SCORE" ->
            ScoreReached 50

        "TURN" ->
            NumberOfTurns 10

        _ ->
            Conquest
