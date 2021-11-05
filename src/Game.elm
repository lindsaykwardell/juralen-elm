module Game exposing (..)

import Components.ZoomButtons.ZoomButtons exposing (zoomButtons)
import Game.Analysis
import Game.Cell exposing (Loc)
import Game.CellType
import Game.Core exposing (..)
import Game.Player exposing (NewPlayer, isHuman)
import Game.PlayerColor
import Game.Scenario as Scenario
import Game.Scoreboard as Scoreboard
import Game.Structure
import Game.TechTree as TechTree exposing (TechDescription, TechLevel(..))
import Game.Unit
import Game.UnitType
import Game.Update exposing (Msg(..), update)
import Html exposing (Attribute, Html, br, button, div, img, span, table, td, text, tr)
import Html.Attributes exposing (class, disabled, src, style)
import Html.Events exposing (onClick, preventDefaultOn)
import Json.Decode as Json


init : List NewPlayer -> Float -> Loc -> ( Model, Cmd Msg )
init newPlayerList aiSpeed loc =
    update InitializeScenario
        { nextId = 1
        , turn = 0
        , grid = []
        , selectedCell = { x = 0, y = 0 }
        , players = []
        , activePlayer = 0
        , units = []
        , selectedUnits = []
        , scenario =
            Scenario.init
                { scenarioType = Scenario.Conquest
                , maxX = loc.x
                , maxY = loc.y
                , players = newPlayerList
                }
        , combat = NoCombat
        , analysisResults = []
        , aiSpeed = aiSpeed
        , mobileTab = DetailsTab
        }


onContextMenu : msg -> Attribute msg
onContextMenu msg =
    preventDefaultOn "contextmenu" (Json.succeed ( msg, True ))


view : Model -> Html Msg
view model =
    div [ class "" ]
        [ div [ class "p-3 lg:fixed bottom-0 left-0 flex lg:flex-row" ]
            [ button
                [ class "py-1 w-2/3 mx-2 lg:p-2 w-full lg:w-[150px] bg-green-500 hover:bg-green-200 disabled:bg-green-300 disabled:hover:bg-green-300 disabled:cursor-default"
                , disabled (not (isHuman model.players model.activePlayer))
                , onClick EndTurn
                ]
                [ text "End Turn" ]
            , button
                [ class "py-1 w-1/3 mx-2 lg:p-2 w-full lg:w-[150px] text-white bg-transparent border-2 border-green-500 hover:border-green-200 hover:bg-[rgba(255,255,255,0.1)]"
                , onClick OpenSettings
                ]
                [ text "Settings" ]
            ]
        , activePlayerCard model
        , div [ class "flex flex-col lg:flex-row" ]
            [ div [ class "w-full lg:w-3/5 p-3" ]
                [ div [ class "max-h-[350px] lg:max-h-[800px] overflow-scroll shadow-inner rounded-lg border-2 border-gray-100 shadow-inner" ]
                    [ table [ class "m-auto" ]
                        (List.map
                            (\row ->
                                tr []
                                    (List.map
                                        (\cell ->
                                            td []
                                                [ div
                                                    [ class
                                                        ("cell "
                                                            ++ (if isInRange model cell then
                                                                    "in-range "

                                                                else
                                                                    ""
                                                               )
                                                            ++ Game.Structure.getCellClass cell.structure
                                                            ++ " "
                                                            ++ Game.Cell.getColorClass cell model.players
                                                        )
                                                    , style "border"
                                                        (if cell.x == model.selectedCell.x && cell.y == model.selectedCell.y then
                                                            "2px solid yellow"

                                                         else
                                                            ""
                                                        )
                                                    , onClick
                                                        (if List.length model.selectedUnits > 0 then
                                                            MoveSelectedUnits cell

                                                         else
                                                            SelectCell { x = cell.x, y = cell.y }
                                                        )
                                                    , onContextMenu (MoveSelectedUnits cell)
                                                    ]
                                                    [ div []
                                                        (List.map
                                                            (\unit ->
                                                                span [ class "unit" ]
                                                                    [ if Game.Unit.isSelected model.selectedUnits unit.id then
                                                                        span []
                                                                            [ text "[ "
                                                                            , img [ src (Game.UnitType.icon unit.unitType), class "unit" ] []
                                                                            , text " ]"
                                                                            ]

                                                                      else
                                                                        span [] [ img [ src (Game.UnitType.icon unit.unitType), class "unit" ] [] ]
                                                                    ]
                                                            )
                                                            (Game.Unit.inCell model.units { x = cell.x, y = cell.y })
                                                        )
                                                    ]
                                                ]
                                        )
                                        row
                                    )
                            )
                            model.grid
                        )
                    ]
                , --zoomButtons [ class "mt-1 flex justify-end" ] []
                  div [ class "flex" ]
                    [ Scoreboard.view model
                    , zoomButtons [ class "mt-1 flex justify-end" ] []
                    ]
                ]
            , div [ class "hidden lg:block lg:w-2/5 p-3" ]
                [ selectedCellCard model
                , buildableUnitList model
                , researchTechList model
                , upgradeCellList model
                , unitsInCellList model
                ]
            , div [ class "w-full text-sm lg:hidden px-3 flex flex-col pb-12" ]
                [ case model.mobileTab of
                    UnitsTab ->
                        unitsInCellList model

                    TechTreeTab ->
                        researchTechList model

                    BuildOptionsTab ->
                        div []
                            [ upgradeCellList model
                            , buildableUnitList model
                            ]

                    DetailsTab ->
                        selectedCellCard model
                ]
            , div [ class "lg:hidden flex bg-gray-700 text-white p-1 fixed bottom-0 w-full" ]
                [ button
                    [ class
                        ("flex-1 p-1"
                            ++ (if model.mobileTab == UnitsTab then
                                    " bg-gray-500 "

                                else
                                    ""
                               )
                        )
                    , onClick (UpdateMobileTab UnitsTab)
                    ]
                    [ text "Units"
                    ]
                , button
                    [ class
                        ("flex-1 p-1"
                            ++ (if model.mobileTab == TechTreeTab then
                                    " bg-gray-500 "

                                else
                                    ""
                               )
                        )
                    , onClick (UpdateMobileTab TechTreeTab)
                    ]
                    [ text "Research"
                    ]
                , button
                    [ class
                        ("flex-1 p-1"
                            ++ (if model.mobileTab == BuildOptionsTab then
                                    " bg-gray-500 "

                                else
                                    ""
                               )
                        )
                    , onClick (UpdateMobileTab BuildOptionsTab)
                    ]
                    [ text "Build"
                    ]
                , button
                    [ class
                        ("flex-1 p-1"
                            ++ (if model.mobileTab == DetailsTab then
                                    " bg-gray-500 "

                                else
                                    ""
                               )
                        )
                    , onClick (UpdateMobileTab DetailsTab)
                    ]
                    [ text "Details"
                    ]
                ]
            ]
        ]


activePlayerCard : Game.Core.Model -> Html Msg
activePlayerCard model =
    if model.activePlayer /= -1 then
        div [ class ("sticky top-0 text-center p-1 text-lg lg:text-xl " ++ Game.Player.getColorClass model.players (Just model.activePlayer)) ]
            [ text ("[ Turn " ++ String.fromInt model.turn ++ " ]")
            , text " "
            , text (Game.Player.getName model.players (Just model.activePlayer) ++ "'s turn")
            , div [ class "flex w-full lg:w-2/3 m-auto" ]
                [ div [ class "flex-1 text-xs lg:txt-sm" ] [ text "Gold: ", text (String.fromInt (currentPlayerStats model).gold) ]
                , div [ class "flex-1 text-xs lg:txt-sm" ] [ text "Actions: ", text (String.fromFloat (currentPlayerStats model).actions) ]
                , div [ class "flex-1 text-xs lg:txt-sm" ] [ text "Farms: ", text (String.fromInt (currentPlayerStats model).farms) ]
                , div [ class "flex-1 text-xs lg:txt-sm" ] [ text "Towns: ", text (String.fromInt (currentPlayerStats model).towns) ]
                , div [ class "flex-1 text-xs lg:txt-sm" ] [ text "Units: ", text (String.fromInt (currentPlayerStats model).units) ]
                ]
            ]

    else
        text ""


selectedCellCard : Game.Core.Model -> Html Msg
selectedCellCard model =
    div [ class "mt-4 border-2 rounded" ]
        [ div
            [ class
                ("p-3 "
                    ++ (case Game.Cell.find model.grid model.selectedCell of
                            Nothing ->
                                ""

                            Just selectedCell ->
                                Game.Cell.getColorClass selectedCell model.players
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
                        (case Game.Cell.find model.grid model.selectedCell of
                            Nothing ->
                                ""

                            Just selectedCell ->
                                Game.CellType.toString selectedCell.cellType
                                    ++ (case selectedCell.structure of
                                            Nothing ->
                                                ""

                                            Just _ ->
                                                " [" ++ Game.Structure.toString selectedCell.structure ++ "]"
                                       )
                        )
                    ]
                , div [ class "flex-1 italic" ]
                    [ text
                        (case Game.Cell.find model.grid model.selectedCell of
                            Nothing ->
                                "Not Controlled"

                            Just selectedCell ->
                                "("
                                    ++ (case selectedCell.controlledBy of
                                            Nothing ->
                                                "Not Controlled"

                                            _ ->
                                                Game.Player.getName model.players selectedCell.controlledBy
                                       )
                                    ++ ")"
                        )
                    ]
                ]
            , div [ class "flex" ]
                [ div [ class "flex-1" ]
                    [ text ("Defense Bonus: " ++ String.fromInt (Game.Cell.atLoc model.grid model.selectedCell |> .defBonus)) ]
                , div [ class "flex-1" ]
                    [ text ("Farms: " ++ String.fromInt (Game.Cell.atLoc model.grid model.selectedCell |> .farms)) ]
                , div [ class "flex-1" ]
                    [ text ("Towers: " ++ String.fromInt (Game.Cell.atLoc model.grid model.selectedCell |> .towers)) ]
                ]
            ]
        ]


buildableUnitList : Game.Core.Model -> Html Msg
buildableUnitList model =
    div []
        (List.map (\buildableUnit -> button [ class "bg-blue-400 hover:bg-blue-200 py-2 px-3 rounded m-2", onClick (BuildUnit buildableUnit) ] [ text ("Build " ++ Game.UnitType.toString buildableUnit) ])
            (case Game.Cell.find model.grid model.selectedCell of
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
                                Game.Structure.canBuild selectedCell.structure (currentPlayerStats model |> .techTree)
            )
        )


researchTechList : Game.Core.Model -> Html Msg
researchTechList model =
    div []
        (Game.Core.getPlayerTechTree model.players model.activePlayer |> TechTree.nextAvailableTech |> List.map techTreeButton)


upgradeCellList : Game.Core.Model -> Html Msg
upgradeCellList model =
    div [ class "p-5" ]
        (if (Game.Cell.atLoc model.grid model.selectedCell |> .controlledBy) /= Just model.activePlayer then
            []

         else
            [ if
                (Game.Cell.atLoc model.grid model.selectedCell |> .defBonus)
                    < Game.Structure.initDef (Game.Cell.atLoc model.grid model.selectedCell |> .structure)
              then
                button [ class "bg-green-400 hover:bg-green-200 py-2 px-3 rounded m-2", onClick (UpgradeCell Game.Analysis.RepairDefense) ] [ text "Repair Defense (1)" ]

              else
                text ""
            , button [ class "bg-green-400 hover:bg-green-200 py-2 px-3 rounded m-2", onClick (UpgradeCell Game.Analysis.BuildFarm) ] [ text "Build Farm (2)" ]
            , button [ class "bg-green-400 hover:bg-green-200 py-2 px-3 rounded m-2", onClick (UpgradeCell Game.Analysis.BuildTower) ] [ text "Build Tower (2)" ]
            ]
        )


unitsInCellList : Game.Core.Model -> Html Msg
unitsInCellList model =
    div [ class "p-5" ]
        (List.map
            (\unit ->
                div
                    [ class
                        ("flex p-2 my-2 rounded text-white pointer"
                            ++ (if Game.Unit.isSelected model.selectedUnits unit.id then
                                    " bg-blue-700 hover:bg-blue-600"

                                else
                                    " bg-gray-700 hover:bg-gray-600"
                               )
                        )
                    , onClick (SelectUnit unit.id)
                    ]
                    [ div [ class "flex flex-col mr-2" ]
                        [ div [ class ("triangle " ++ Game.PlayerColor.toString (Game.Player.getColor model.players unit.controlledBy)) ] []
                        , div [ class ("triangle " ++ Game.PlayerColor.toString (Game.Player.getColor model.players unit.controlledBy)) ] []
                        ]
                    , div [ class "w-1/3 text-left" ] [ text (Game.UnitType.toString unit.unitType) ]
                    , div [ class "flex-1" ] [ text "Atk: ", text (String.fromInt unit.attack) ]
                    , div [ class "flex-1" ] [ text "HP: ", text (String.fromInt unit.health) ]
                    , div [ class "flex-1" ] [ text "Moves: ", text (String.fromInt unit.movesLeft) ]
                    ]
            )
            (Game.Unit.inCell model.units model.selectedCell)
        )


techTreeButton : TechDescription -> Html Msg
techTreeButton tech =
    button
        [ class "bg-yellow-400 hover:bg-yellow-200 py-2 px-3 rounded m-2"
        , onClick (ResearchTech tech)
        ]
        [ text (tech.name ++ " (" ++ String.fromInt tech.cost ++ ")")
        , br [] []
        , text tech.description
        ]
