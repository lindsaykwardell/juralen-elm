module Game exposing (..)

import FontAwesome.Icon as Icon
import FontAwesome.Solid as Icon
import Game.Action
import Game.Cell
import Game.CellType
import Game.Core exposing (..)
import Game.Grid
import Game.History as History exposing (History)
import Game.Level
import Game.Loc as Loc
import Game.Player exposing (NewPlayer)
import Game.PlayerColor
import Game.Scenario as Scenario
import Game.Scoreboard as Scoreboard
import Game.Structure
import Game.TechTree as TechTree exposing (TechDescription, TechLevel(..))
import Game.Unit exposing (controlledBy)
import Game.UnitType
import Game.Update exposing (Msg(..), update)
import Game.View.CombatModal as CombatModal
import Game.View.Grid as Grid
import Game.View.PurchaseButton as PurchaseButton
import Html exposing (Attribute, Html, br, button, div, h2, span, text)
import Html.Attributes exposing (class, disabled)
import Html.Events exposing (onClick, preventDefaultOn)
import Json.Decode as Decode
import Sort.Dict


init :
    { newPlayerList : List NewPlayer
    , aiSpeed : Float
    , size : { x : Int, y : Int }
    , scenarioType : Scenario.ScenarioType
    }
    -> ( Model, Cmd Msg )
init { newPlayerList, aiSpeed, size, scenarioType } =
    update InitializeScenario
        { nextId = 1
        , turn = 0
        , grid = Sort.Dict.empty Game.Grid.sorter
        , openCell = Loc.at 0 0
        , selectedCell = Loc.at 0 0
        , players = []
        , activePlayer = 0
        , units = []
        , selectedUnits = []
        , scenario =
            Scenario.init
                { scenarioType = scenarioType

                -- n - 1 is for adjusting for lists starting at 0
                , maxX =
                    (if size.x <= 0 then
                        9

                     else
                        size.x
                    )
                        - 1
                , maxY =
                    (if size.y <= 0 then
                        9

                     else
                        size.y
                    )
                        - 1
                , players = newPlayerList
                }
        , combat = NoCombat
        , aiSpeed = aiSpeed
        , mobileTab = DetailsTab
        , actionHistory = []
        }


onContextMenu : msg -> Attribute msg
onContextMenu msg =
    preventDefaultOn "contextmenu" (Decode.succeed ( msg, True ))


view : Model -> Html Msg
view model =
    div [ class "" ]
        [ case model.combat of
            NoCombat ->
                text ""

            Combat combatModel ->
                CombatModal.view combatModel
        , div [ class "p-3 lg:fixed bottom-0 left-0 flex lg:flex-row bg-[#333]" ]
            [ button
                [ class "py-1 w-2/3 mx-2 lg:p-2 w-full lg:w-[150px] bg-green-500 hover:bg-green-200 disabled:bg-green-300 disabled:hover:bg-green-300 disabled:cursor-default"
                , disabled <| not <| .isHuman <| Game.Player.get model.players model.activePlayer
                , onClick EndTurn
                ]
                [ text "End Turn" ]
            , button
                [ class "py-1 w-1/3 mx-2 lg:p-2 w-full lg:w-[150px] text-white bg-transparent border-2 border-green-500 hover:border-green-200 hover:bg-[rgba(255,255,255,0.1)]"
                , onClick OpenSettings
                ]
                [ text "Settings" ]
            , button
                [ class "py-1 w-1/3 mx-2 lg:p-2 w-full lg:w-[150px] text-white bg-transparent border-2 border-green-500 hover:border-green-200 hover:bg-[rgba(255,255,255,0.1)]"
                , onClick SaveGame
                ]
                [ text "Save Game" ]
            ]
        , activePlayerCard model
        , div [ class "flex flex-col lg:flex-row" ]
            [ div [ class "w-full lg:w-3/5 p-3" ]
                [ Grid.view
                    model.grid
                    { isInRange = isInRange model
                    , selectedCell = model.openCell
                    , getCellColor = \cell -> Game.Cell.getColorClass cell model.players
                    , onCellClick =
                        \cell ->
                            if List.length model.selectedUnits > 0 then
                                MoveSelectedUnits cell

                            else
                                SelectCell cell.loc
                    , unitsInCell = \cell -> Game.Unit.inCell model.units cell.loc
                    , selectedUnits = model.selectedUnits
                    , zoomIn = ZoomIn
                    , zoomOut = ZoomOut
                    }
                , div [ class "flex pt-2" ]
                    [ Scoreboard.view model
                    ]
                ]
            , div [ class "hidden lg:block lg:w-2/5 p-3 flex flex-col" ]
                [ selectedCellCard model
                , div [ class "flex flex-wrap h-36 xl:h-20" ]
                    (if Game.Player.get model.players model.activePlayer |> .isHuman then
                        buildableUnitList model
                            ++ researchTechList model
                            ++ upgradeCellList model

                     else
                        []
                    )
                , unitsInCellList model
                , historyView model.actionHistory
                ]
            , div [ class "w-full text-sm lg:hidden px-3 flex flex-col pb-12" ]
                [ case model.mobileTab of
                    UnitsTab ->
                        unitsInCellList model

                    PurchaseTab ->
                        div []
                            (buildableUnitList model
                                ++ researchTechList model
                                ++ upgradeCellList model
                            )

                    DetailsTab ->
                        selectedCellCard model
                ]
            , div [ class "lg:hidden flex bg-gray-700 text-white p-1 fixed bottom-[20px] w-full" ]
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
                            ++ (if model.mobileTab == PurchaseTab then
                                    " bg-gray-500 "

                                else
                                    ""
                               )
                        )
                    , onClick (UpdateMobileTab PurchaseTab)
                    ]
                    [ text "Purchase"
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
        , if model.activePlayer == -1 then
            div [ class "mb-20 text-white w-11/12 m-auto border-t border-white my-4" ]
                [ h2 [ class "text-3xl py-4" ] [ text "Game Overview" ]
                , div [ class "h-[175px] md:h-[300px] p-3 border border-2 border-white" ]
                    [ Scoreboard.graph model
                    ]
                , Scoreboard.stats model
                ]

          else
            text ""
        ]


activePlayerCard : Game.Core.Model -> Html Msg
activePlayerCard model =
    if model.activePlayer /= -1 then
        div
            [ class
                ("sticky top-0 text-center p-1 text-lg lg:text-xl "
                    ++ (Game.Player.get model.players model.activePlayer
                            |> .color
                            |> Game.PlayerColor.toClass
                       )
                    ++ (if
                            Game.Player.get model.players model.activePlayer
                                |> .color
                                |> Game.PlayerColor.isDark
                        then
                            " text-white"

                        else
                            ""
                       )
                )
            ]
            [ text ("[ Turn " ++ String.fromInt model.turn ++ " ]")
            , text " "
            , text ((Game.Player.get model.players model.activePlayer |> .name) ++ "'s turn")
            , div [ class "flex w-full lg:w-2/3 m-auto" ]
                [ div [ class "flex-1 text-xs lg:txt-sm" ] [ text "Gold: ", text (String.fromInt (currentPlayerStats model).gold) ]
                , div [ class "flex-1 text-xs lg:txt-sm" ] [ text "Actions: ", text (String.fromFloat (currentPlayerStats model).actions) ]
                , div [ class "flex-1 text-xs lg:txt-sm" ] [ text "Farms: ", text (String.fromInt <| (currentPlayerStats model).farms ()) ]
                , div [ class "flex-1 text-xs lg:txt-sm" ] [ text "Towns: ", text (String.fromInt <| (currentPlayerStats model).towns ()) ]
                , div [ class "flex-1 text-xs lg:txt-sm" ] [ text "Units: ", text (String.fromInt (currentPlayerStats model).units) ]
                ]
            ]

    else
        text ""


selectedCellCard : Game.Core.Model -> Html Msg
selectedCellCard model =
    div [ class "mt-4 border-2" ]
        [ div
            [ class
                ("p-3 "
                    ++ (case Sort.Dict.get model.openCell model.grid of
                            Nothing ->
                                ""

                            Just openCell ->
                                Game.Cell.getColorClass openCell model.players
                       )
                    ++ (if
                            Sort.Dict.get model.openCell model.grid
                                |> Maybe.andThen .controlledBy
                                |> Maybe.map (Game.Player.get model.players)
                                |> Maybe.map .color
                                |> Maybe.map Game.PlayerColor.isDark
                                |> Maybe.withDefault False
                        then
                            " text-white"

                        else
                            ""
                       )
                )
            ]
            [ text (String.fromInt <| Loc.getX model.openCell)
            , text ", "
            , text (String.fromInt <| Loc.getY model.openCell)
            , br [] []
            , div [ class "flex" ]
                [ div [ class "flex-1" ]
                    [ text
                        (case Sort.Dict.get model.openCell model.grid of
                            Nothing ->
                                ""

                            Just openCell ->
                                Game.CellType.toString openCell.cellType
                                    ++ (case openCell.structure of
                                            Game.Structure.None ->
                                                ""

                                            _ ->
                                                " [" ++ Game.Structure.toString openCell.structure ++ "]"
                                       )
                        )
                    ]
                , div [ class "flex-1 italic" ]
                    [ text
                        (case Sort.Dict.get model.openCell model.grid of
                            Nothing ->
                                "Not Controlled"

                            Just openCell ->
                                "("
                                    ++ (case openCell.controlledBy of
                                            Nothing ->
                                                "Not Controlled"

                                            Just playerId ->
                                                Game.Player.get model.players playerId |> .name
                                       )
                                    ++ ")"
                        )
                    ]
                ]
            , div [ class "flex" ]
                [ div [ class "flex-1" ]
                    [ text ("Defense Bonus: " ++ String.fromInt (Sort.Dict.get model.openCell model.grid |> Maybe.map .defBonus |> Maybe.withDefault 0)) ]
                , div [ class "flex-1" ]
                    [ text ("Farms: " ++ String.fromInt (Sort.Dict.get model.openCell model.grid |> Maybe.map .farms |> Maybe.withDefault 0)) ]
                , div [ class "flex-1" ]
                    [ text ("Towers: " ++ String.fromInt (Sort.Dict.get model.openCell model.grid |> Maybe.map .towers |> Maybe.withDefault 0)) ]
                ]
            ]
        ]


buildableUnitList : Game.Core.Model -> List (Html Msg)
buildableUnitList model =
    List.map
        (\buildableUnit ->
            PurchaseButton.blue
                { icon = Game.UnitType.icon buildableUnit
                , description =
                    "Build "
                        ++ Game.UnitType.toString
                            buildableUnit
                , cost = Game.UnitType.cost buildableUnit
                , onClick = BuildUnit buildableUnit
                , disabled = Game.UnitType.cost buildableUnit > (currentPlayerStats model |> .gold)
                }
        )
        (case Sort.Dict.get model.openCell model.grid of
            Nothing ->
                []

            Just openCell ->
                case openCell.controlledBy of
                    Nothing ->
                        []

                    Just controlledBy ->
                        if controlledBy /= model.activePlayer then
                            []

                        else
                            Game.Structure.canBuild openCell.structure (currentPlayerStats model |> .techTree)
        )


researchTechList : Game.Core.Model -> List (Html Msg)
researchTechList model =
    if model.activePlayer == -1 then
        []

    else
        Game.Core.getPlayerTechTree model.players model.activePlayer |> TechTree.nextAvailableTech |> List.map (techTreeButton model)


upgradeCellList : Game.Core.Model -> List (Html Msg)
upgradeCellList model =
    Sort.Dict.get model.openCell model.grid
        |> Maybe.map
            (\cell ->
                if cell.controlledBy /= Just model.activePlayer then
                    []

                else if cell.structure /= Game.Structure.None then
                    [ PurchaseButton.green
                        { icon = "/img/repair-defense.png"
                        , description = "Repair Defense"
                        , cost = 1
                        , onClick = UpgradeCell Game.Action.RepairDefense
                        , disabled =
                            cell.defBonus
                                >= Game.Structure.initDef cell.structure
                                || (currentPlayerStats model |> .gold)
                                < 1
                        }
                    , PurchaseButton.green
                        { icon = "/img/farm.svg"
                        , description = "Build Farm"
                        , cost = 2
                        , onClick = UpgradeCell Game.Action.BuildFarm
                        , disabled = (cell.towers + cell.farms) >= Game.Structure.maxUpgradeCount cell.structure || (currentPlayerStats model |> .gold) < 2
                        }
                    , PurchaseButton.green
                        { icon = "/img/tower.png"
                        , description = "Build Tower"
                        , cost = 2
                        , onClick = UpgradeCell Game.Action.BuildTower
                        , disabled = (cell.towers + cell.farms) >= Game.Structure.maxUpgradeCount cell.structure || (currentPlayerStats model |> .gold) < 2
                        }
                    ]

                else
                    []
            )
        |> Maybe.withDefault []


unitsInCellList : Game.Core.Model -> Html Msg
unitsInCellList model =
    div [ class "p-5 flex-grow h-[215px] xl:h-[300px] overflow-y-scroll" ]
        (List.map
            (\unit ->
                div
                    [ class
                        ("flex p-2 my-2 text-white pointer"
                            ++ (if Game.Unit.isSelected model.selectedUnits unit.id then
                                    " bg-blue-700 hover:bg-blue-600"

                                else
                                    " bg-gray-700 hover:bg-gray-600"
                               )
                        )
                    , onClick (SelectUnit unit.id)
                    ]
                    [ div [ class "flex flex-col mr-2" ]
                        [ div [ class ("triangle " ++ Game.PlayerColor.toString (Game.Player.get model.players unit.controlledBy |> .color)) ] []
                        , div [ class ("triangle " ++ Game.PlayerColor.toString (Game.Player.get model.players unit.controlledBy |> .color)) ] []
                        ]
                    , div [ class "w-1/3 text-left flex items-center" ]
                        [ span [ class "w-16" ] [ text (Game.UnitType.toString unit.unitType) ]
                        , span [ class "text-yellow-400 text-xs flex" ] (List.repeat (Game.Level.currentLevel unit.level) (Icon.viewIcon Icon.star))
                        ]
                    , div [ class "flex-1" ] [ text "Atk: ", text (String.fromInt unit.attack) ]
                    , div [ class "flex-1" ] [ text "HP: ", text (String.fromInt unit.health) ]
                    , div [ class "flex-1" ] [ text "Moves: ", text (String.fromInt unit.movesLeft) ]
                    ]
            )
            (Game.Unit.inCell model.units model.openCell)
        )


techTreeButton : Model -> TechDescription -> Html Msg
techTreeButton model tech =
    PurchaseButton.yellow
        { icon = tech.icon
        , description = tech.name ++ ". " ++ tech.description
        , cost = tech.cost
        , onClick = ResearchTech tech
        , disabled = (currentPlayerStats model |> .gold) < tech.cost
        }


historyView : List History -> Html Msg
historyView history =
    div [ class "w-full italic text-left text-gray-300 bg-gray-800 p-2 overflow-y-scroll h-[200px]" ]
        (history
            -- |> List.take 5
            |> List.map
                (\item ->
                    div [ class "flex" ]
                        [ div [ class "w-[75px]" ]
                            [ text <| "Turn " ++ (item.turn |> String.fromInt) ]
                        , div []
                            [ text <| History.toString item ]
                        ]
                )
        )
