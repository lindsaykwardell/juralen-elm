module Main exposing (..)

import Array
import Browser
import Html exposing (Html, br, button, div, span, table, td, text, tr)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import Juralen.Types.Cell exposing (Cell, buildStructure, controlledBy, findCell, findCellWithoutStructure, generateCell, getCellColor, getStructure)
import Juralen.Types.Loc exposing (Loc)
import Juralen.Types.Player exposing (NewPlayer, Player, findPlayer, findPlayerColor, generatePlayer)
import Juralen.Types.Unit exposing (Unit, buildUnit, findUnitsInCell)
import Juralen.Types.UnitType exposing (UnitType)
import Random



---- MODEL ----


type alias Model =
    { nextId : Int
    , grid : List (List Cell)
    , selectedCell : Loc
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
    }


init : ( Model, Cmd Msg )
init =
    update (RollNextCell { x = 0, y = 0 })
        { nextId = 1
        , grid = []
        , selectedCell = { x = 0, y = 0 }
        , players = []
        , activePlayer = 0
        , units = []
        , init =
            { maxX = 8
            , maxY = 8
            , currentX = 0
            , currentY = 0
            , finished = False
            , newPlayers =
                [ { name = "Lindsay", isHuman = True, color = Juralen.Types.Player.Red }
                , { name = "Ilthanen Juralen", isHuman = True, color = Juralen.Types.Player.Blue }
                , { name = "Velsyph", isHuman = True, color = Juralen.Types.Player.Green }
                , { name = "Dakh", isHuman = True, color = Juralen.Types.Player.Yellow }
                ]
            }
        }



---- UPDATE ----


randomDefinedMax : Int -> Random.Generator Int
randomDefinedMax max =
    Random.int 0 max


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
    | SelectCell Loc
    | BuildUnit UnitType


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
                    if model.init.currentX > model.init.maxX then
                        True

                    else
                        False

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
                        model.grid ++ [ [ generateCell loc roll ] ]

                    else
                        List.map
                            (\row ->
                                if List.length row > model.init.maxY then
                                    row

                                else
                                    row ++ [ generateCell loc roll ]
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
                            generatePlayer newPlayer model.nextId

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
                    findCellWithoutStructure model.grid loc
            in
            case cell of
                Nothing ->
                    update (RollStartingLocX player nextPlayers) model

                Just realCell ->
                    let
                        newGrid =
                            replaceCell model.grid (controlledBy (buildStructure realCell "todo") player)

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
                            [ buildUnit Juralen.Types.UnitType.Soldier player loc model.nextId, buildUnit Juralen.Types.UnitType.Soldier player loc (model.nextId + 1), buildUnit Juralen.Types.UnitType.Soldier player loc (model.nextId + 2) ]

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

        StartTurn player ->
            ( { model | activePlayer = player.id }, Cmd.none )

        SelectCell loc ->
            ( { model | selectedCell = loc }, Cmd.none )

        BuildUnit unitType ->
            -- let
            --     newUnit : Unit
            --     newUnit =
            --         buildUnit unitType model.player model.selectedCell model.nextId
            -- in
            ( model, Cmd.none )


replaceCell : List (List Cell) -> Cell -> List (List Cell)
replaceCell grid newCell =
    List.map
        (\row ->
            List.map
                (\cell ->
                    if cell.x == newCell.x && cell.y == newCell.y then
                        newCell

                    else
                        cell
                )
                row
        )
        grid



---- VIEW ----


view : Model -> Html Msg
view model =
    div [ class "flex" ]
        [ table [ class "w-3/5" ]
            (List.map
                (\row ->
                    tr []
                        (List.map
                            (\cell ->
                                td []
                                    [ div
                                        [ class ("cell " ++ getCellColor cell model.players)
                                        , style "border"
                                            (if cell.x == model.selectedCell.x && cell.y == model.selectedCell.y then
                                                "2px solid yellow"

                                             else
                                                ""
                                            )
                                        , onClick (SelectCell { x = cell.x, y = cell.y })
                                        ]
                                        [ text (getStructure cell.structure)
                                        , br [] []
                                        , div [] (List.map (\unit -> span [ class "unit" ] [ text unit.short ]) (findUnitsInCell model.units cell))
                                        ]
                                    ]
                            )
                            row
                        )
                )
                model.grid
            )
        , div [ class "w-2/5 m-3" ]
            [ div [ class ("text-center p-3 " ++ findPlayerColor model.players (Just model.activePlayer)) ]
                [ text (findPlayer model.players (Just model.activePlayer))
                ]
            , div [ class "mt-4 border-2 rounded" ]
                [ div
                    [ class
                        ("p-3 "
                            ++ (case findCell model.grid model.selectedCell of
                                    Nothing ->
                                        ""

                                    Just selectedCell ->
                                        getCellColor selectedCell model.players
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
                                (case findCell model.grid model.selectedCell of
                                    Nothing ->
                                        ""

                                    Just selectedCell ->
                                        selectedCell.terrain
                                )
                            ]
                        , div [ class "flex-1 italic" ]
                            [ text
                                (case findCell model.grid model.selectedCell of
                                    Nothing ->
                                        "Not Controlled"

                                    Just selectedCell ->
                                        "("
                                            ++ (case selectedCell.controlledBy of
                                                    Nothing ->
                                                        "Not Controlled"

                                                    _ ->
                                                        findPlayer model.players selectedCell.controlledBy
                                               )
                                            ++ ")"
                                )
                            ]
                        ]
                    ]
                ]
            , div []
                (List.map (\buildableUnit -> button [ class "build-unit" ] [ text ("Build " ++ Juralen.Types.UnitType.toString buildableUnit) ])
                    (case findCell model.grid model.selectedCell of
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
                                        case selectedCell.structure of
                                            Nothing ->
                                                []

                                            Just structure ->
                                                structure.buildUnits
                    )
                )
            ]
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
