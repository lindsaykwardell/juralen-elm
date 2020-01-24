module Main exposing (..)

import Browser
import Html exposing (Html, br, div, table, td, text, tr)
import Html.Attributes exposing (class, style)
import Juralen.Types.Cell exposing (Cell, buildStructure, controlledBy, generateCell, getStructure)
import Juralen.Types.Loc exposing (Loc)
import Juralen.Types.Player exposing (NewPlayer, Player, findPlayer, findPlayerColor, generatePlayer)
import Random



---- MODEL ----


type alias Model =
    { nextId : Int
    , grid : List (List Cell)
    , players : List Player
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
        , players = []
        , init =
            { maxX = 9
            , maxY = 9
            , currentX = 0
            , currentY = 0
            , finished = False
            , newPlayers =
                [ { name = "Lindsay", isHuman = True, color = "red" }
                , { name = "Ilthanen Juralen", isHuman = True, color = "green" }
                ]
            }
        }



---- UPDATE ----


randomTileNumber : Random.Generator Int
randomTileNumber =
    Random.int 0 101


randomStartLoc : Int -> Random.Generator Int
randomStartLoc max =
    Random.int 0 max


type Msg
    = GenerateNextCell Loc Int
    | RollNextCell Loc
    | GenerateNextPlayer NewPlayer
    | GenerateStartingLoc Player (List Player) Loc
    | RollStartingLocX Player (List Player)
    | RollStartingLocY Player (List Player) Int
    | MakeLocFromRolls Player (List Player) Int Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GenerateNextCell loc roll ->
            let
                finished =
                    if model.init.currentX >= model.init.maxX && model.init.currentY >= model.init.maxY then
                        True

                    else
                        False

                nextX =
                    if model.init.currentY == model.init.maxY && finished == False then
                        model.init.currentX + 1

                    else
                        model.init.currentX

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

                    else if model.init.currentY == model.init.maxY then
                        model.grid ++ [ [ generateCell loc roll ] ]

                    else
                        List.map
                            (\row ->
                                if List.length row == model.init.maxY then
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
                ( model, Random.generate (GenerateNextCell loc) randomTileNumber )

            else if List.length model.init.newPlayers > 0 then
                let
                    nextNewPlayer =
                        case List.head model.init.newPlayers of
                            Nothing ->
                                { name = "Null", isHuman = False, color = "#FFF" }

                            Just theNextNewPlayer ->
                                theNextNewPlayer
                in
                update (GenerateNextPlayer nextNewPlayer) model

            else
                ( model, Cmd.none )

        GenerateNextPlayer newPlayer ->
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
                let
                    nextNewPlayer =
                        case List.head remainingNewPlayers of
                            Nothing ->
                                { name = "Null", isHuman = False, color = "#FFF" }

                            Just theNextNewPlayer ->
                                theNextNewPlayer
                in
                update (GenerateNextPlayer nextNewPlayer) newModel

        GenerateStartingLoc player nextPlayers loc ->
            let
                cell : Maybe Cell
                cell =
                    case List.head (List.filter (\row -> List.length (List.filter (\innerCell -> innerCell.x == loc.x && innerCell.y == loc.y && innerCell.structure == Nothing) row) > 0) model.grid) of
                        Nothing ->
                            Nothing

                        Just row ->
                            List.head (List.filter (\innerCell -> innerCell.x == loc.x && innerCell.y == loc.y && innerCell.structure == Nothing) row)
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

                        nextModel =
                            { model | grid = newGrid }
                    in
                    case nextPlayer of
                        Nothing ->
                            ( nextModel, Cmd.none )

                        Just theNextPlayer ->
                            update (RollStartingLocX theNextPlayer remainingPlayers) nextModel

        RollStartingLocX player nextPlayers ->
            ( model, Random.generate (RollStartingLocY player nextPlayers) (randomStartLoc 9) )

        RollStartingLocY player nextPlayers xVal ->
            ( model, Random.generate (MakeLocFromRolls player nextPlayers xVal) (randomStartLoc 9) )

        MakeLocFromRolls player nextPlayers xVal yVal ->
            update (GenerateStartingLoc player nextPlayers { x = xVal, y = yVal }) model


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
    div [] [ table [] (List.map (\row -> tr [] (List.map (\cell -> td [] [ div [ class "cell", style "background" (findPlayerColor model.players cell.controlledBy) ] [ text cell.terrain, br [] [], text (getStructure cell.structure) ] ]) row)) model.grid) ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
