module Main exposing (..)

import Array
import Browser
import Html exposing (Html, Attribute, br, button, div, span, table, td, text, tr)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick, on, preventDefaultOn)
import Juralen.Cell exposing (Cell, Loc)
import Juralen.CellType exposing (CellType)
import Juralen.Grid exposing (Grid)
import Juralen.Player exposing (NewPlayer, Player)
import Juralen.PlayerColor exposing (PlayerColor)
import Juralen.Resources exposing (Resources)
import Juralen.Structure exposing (Structure)
import Juralen.Unit exposing (Unit)
import Juralen.UnitType exposing (UnitType)
import Random
import Json.Decode as Json



---- MODEL ----


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
        , selectedUnits = []
        , init =
            { maxX = 8
            , maxY = 8
            , currentX = 0
            , currentY = 0
            , finished = False
            , newPlayers =
                [ { name = "Lindsay", isHuman = True, color = Juralen.PlayerColor.Red }
                , { name = "Ilthanen Juralen", isHuman = True, color = Juralen.PlayerColor.Blue }
                , { name = "Velsyph", isHuman = True, color = Juralen.PlayerColor.Green }
                , { name = "Dakh", isHuman = True, color = Juralen.PlayerColor.Yellow }
                ]
            }
        }



---- FUNCTIONS ----


randomDefinedMax : Int -> Random.Generator Int
randomDefinedMax max =
    Random.int 0 max




type alias CurrentPlayerStats =
    { gold : Int
    , actions : Float
    , farms : Int
    , towns : Int
    , units : Int
    }


currentPlayerStats : Model -> CurrentPlayerStats
currentPlayerStats model =
    { gold = (Juralen.Player.getResources model.players model.activePlayer).gold
    , actions = (Juralen.Player.getResources model.players model.activePlayer).actions
    , farms = Juralen.Grid.farmCountControlledBy model.grid model.activePlayer
    , towns = Juralen.Grid.townCountControlledBy model.grid model.activePlayer
    , units = List.length (List.filter (\unit -> unit.controlledBy == model.activePlayer) model.units)
    }


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
        stats = currentPlayerStats model

        unitCost = Juralen.UnitType.cost unitType
    in
        if stats.units < stats.farms && unitCost <= stats.gold 
        then
            True

        else
            False
    


isInRange : Model -> Cell -> Bool
isInRange model cell =
    if
        List.length model.selectedUnits
            > 0
            && model.selectedCell /= {x = cell.x, y = cell.y}
            && Juralen.CellType.isPassable cell.cellType
            && (currentPlayerStats model).actions
            >= (Basics.toFloat (Juralen.Cell.getDistance model.selectedCell { x = cell.x, y = cell.y }) * (getMoveCost model))
    then
        True

    else
        False


getNextActivePlayer : Model -> List Player -> Int -> Player
getNextActivePlayer model players playerId =
    let
        player : Maybe Player
        player = List.head players

        remainingPlayers : List Player
        remainingPlayers = 
            case List.tail players of 
                Nothing ->
                    []

                Just playerList ->
                    playerList
    in
        case player of
            Nothing ->
                Juralen.Player.empty
            
            Just aPlayer ->
                if aPlayer.id == playerId then
                    case List.head remainingPlayers of
                        Nothing ->
                            case List.head model.players of
                                Nothing ->
                                    Juralen.Player.empty

                                Just nextPlayer ->
                                    nextPlayer

                        Just nextPlayer ->
                            nextPlayer

                else
                    getNextActivePlayer model remainingPlayers playerId
    

gainResources : CurrentPlayerStats -> Resources -> Resources
gainResources stats resources =
    let
        gold = resources.gold + stats.farms

        actions = Basics.toFloat stats.towns + resources.actions
    in
        {gold = gold
        , actions = actions}


onContextMenu : msg -> Attribute msg
onContextMenu msg =
    preventDefaultOn "contextmenu" (Json.succeed (msg, True))

---- UPDATE ----


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
    | SelectUnit Int
    | MoveSelectedUnits Cell
    | EndTurn


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
                    let
                        newGrid =
                            Juralen.Grid.replaceCell model.grid (Juralen.Cell.updateControl (Juralen.Cell.buildStructure realCell "todo") player.id)

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

        StartTurn player ->
            ( { model | activePlayer = player.id }, Cmd.none )

        SelectCell loc ->
            ( { model | selectedCell = loc }, Cmd.none )

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
                ( { model | nextId = nextId, units = newUnitList, players = newPlayerList }, Cmd.none )

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
            if isInRange model cell == False
                then
                    ( model, Cmd.none)

                else
                    let
                        newResources : Resources
                        newResources = Juralen.Resources.useActions (Juralen.Player.getResources model.players model.activePlayer) (Basics.toFloat (Juralen.Cell.getDistance model.selectedCell { x = cell.x, y = cell.y }) * (getMoveCost model))

                        newPlayerList = List.map (\player -> if player.id == model.activePlayer then { player | resources = newResources } else player) model.players

                        newUnitList = List.map (\unit -> 
                            if Juralen.Unit.isSelected model.selectedUnits unit.id 
                            && unit.x == model.selectedCell.x 
                            && unit.y == model.selectedCell.y 
                                then 
                                    { unit | x = cell.x, y = cell.y, movesLeft = unit.movesLeft - 1} 

                                else 
                                    unit) model.units

                        newSelectedCell = {x = cell.x, y = cell.y}

                        newCell = if cell.cellType == Juralen.CellType.Plains then Juralen.Cell.updateControl cell model.activePlayer else cell

                        newGrid = Juralen.Grid.replaceCell model.grid newCell
                    in
                        ( { model | grid = newGrid, players = newPlayerList, units = newUnitList, selectedCell = newSelectedCell, selectedUnits = []}, Cmd.none)

        EndTurn ->
            let
                nextActivePlayer : Player
                nextActivePlayer = getNextActivePlayer model model.players model.activePlayer

                updatedPlayers : List Player
                updatedPlayers = List.map (\player -> 
                    if player.id == nextActivePlayer.id
                        then 
                            { nextActivePlayer | resources = gainResources (currentPlayerStats model) nextActivePlayer.resources } 
                        
                        else 
                            player
                    ) model.players
            in
                ( { model | players = updatedPlayers, activePlayer = nextActivePlayer.id }, Cmd.none)
            
                    



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
                                        [ text (
                                            if isInRange model cell 
                                                then 
                                                    "## " ++ Juralen.CellType.toString cell.cellType ++ " ##" 
                                                
                                                else 
                                                    Juralen.CellType.toString cell.cellType ) 
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
        , div [ class "w-2/5 m-3" ]
            [ div [ class "p-3"] [
                button [class "py-2 w-full bg-green-500 hover:bg-green-400", onClick EndTurn] [text "End Turn"]
            ]
            , div [ class ("text-center p-3 " ++ Juralen.Player.getColorClass model.players (Just model.activePlayer)) ]
                [ text (Juralen.Player.getName model.players (Just model.activePlayer))
                , div [ class "flex" ]
                    [ div [ class "flex-1 p-2" ] [ text "Gold: ", text (String.fromInt (currentPlayerStats model).gold) ]
                    , div [ class "flex-1 p-2" ] [ text "Actions: ", text (String.fromFloat (currentPlayerStats model).actions) ]
                    , div [ class "flex-1 p-2" ] [ text "Farms: ", text (String.fromInt (currentPlayerStats model).farms) ]
                    , div [ class "flex-1 p-2" ] [ text "Towns: ", text (String.fromInt (currentPlayerStats model).towns) ]
                    , div [ class "flex-1 p-2" ] [ text "Units: ", text (String.fromInt (currentPlayerStats model).units) ]
                    ]
                ]
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
                                        Juralen.CellType.toString selectedCell.cellType
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
                    ]
                ]
            , div []
                (List.map (\buildableUnit -> button [ class "build-unit", onClick (BuildUnit buildableUnit) ] [ text ("Build " ++ Juralen.UnitType.toString buildableUnit) ])
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
                                        Juralen.Structure.canBuild selectedCell.structure
                    )
                )
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
                            [ div [ class "w-1/3 text-left" ] [ text (Juralen.UnitType.toString unit.unitType) ]
                            , div [ class "flex-1" ] [ text "Atk: ", text (String.fromInt unit.attack) ]
                            , div [ class "flex-1" ] [ text "HP: ", text (String.fromInt unit.health) ]
                            , div [ class "flex-1" ] [ text "Moves: ", text (String.fromInt unit.movesLeft) ]
                            ]
                    )
                    (Juralen.Unit.inCell model.units model.selectedCell)
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
