module Game.View.CombatModal exposing (view)

import Components.Modal as Modal
import Game.CellType
import Game.Combat as Combat
import Game.Loc as Loc
import Game.Player exposing (Player)
import Game.Unit exposing (Unit)
import Game.UnitType
import Game.Update exposing (Msg(..))
import Game.View.Flag as Flag
import Html exposing (Html, button, div, h1, h2, img, text)
import Html.Attributes exposing (class, src, style)
import Html.Events exposing (onClick)


view :
    Combat.Model
    -> Html Msg
view model =
    let
        attackingUnits =
            List.filter (\u -> u.controlledBy == model.attackingPlayer.id) model.units

        defendingUnits =
            List.filter (\u -> u.controlledBy == model.defendingPlayer.id) model.units

        combatEnded =
            List.length attackingUnits <= 0 || List.length defendingUnits <= 0
    in
    Modal.view
        { show = True
        , onClose =
            if combatEnded then
                GotCombatMsg Combat.ExitCombat

            else
                NoOp
        , content =
            div [ class ("text-white p-2 " ++ Game.CellType.getColorClass model.cell.cellType) ]
                [ h1 []
                    [ text "( "
                    , text (String.fromInt (Loc.getX model.cell.loc))
                    , text ", "
                    , text (String.fromInt (Loc.getY model.cell.loc))
                    , text " )"
                    ]
                , div [ class "flex" ]
                    [ unitListDisplay model.attackingPlayer attackingUnits model.attacker
                    , if model.defBonus > 0 then
                        div [ class "flex" ]
                            (List.range 0 model.defBonus
                                |> List.map
                                    (\i ->
                                        div [ class "w-2 odd:bg-gray-800 bg-gray-700 flex flex-col" ]
                                            (List.range 0 ((List.length model.units + List.length model.deadUnits) * 6)
                                                |> List.map
                                                    (\j ->
                                                        div
                                                            [ class <|
                                                                "flex-1 "
                                                                    ++ (if i == 0 then
                                                                            "bg-gray-800 odd:bg-gray-700"

                                                                        else if modBy i j == 0 then
                                                                            "bg-gray-600"

                                                                        else
                                                                            ""
                                                                       )
                                                            ]
                                                            []
                                                    )
                                            )
                                    )
                            )

                      else
                        text ""
                    , unitListDisplay model.defendingPlayer defendingUnits model.defender
                    ]
                , if List.length attackingUnits <= 0 then
                    h2 [ class "text-center" ]
                        [ text <| model.defendingPlayer.name ++ " wins!" ]

                  else if List.length defendingUnits <= 0 then
                    h2 [ class "text-center" ] [ text <| model.attackingPlayer.name ++ " wins!" ]

                  else
                    text ""
                , if combatEnded then
                    div [ class "text-center" ]
                        [ button [ class "px-6 py-2 bg-gray-700 hover:bg-gray-800", onClick (GotCombatMsg Combat.ExitCombat) ] [ text "Close" ] ]

                  else
                    text ""
                ]
        }


unitListDisplay : Player -> List Unit -> Unit -> Html msg
unitListDisplay player units focusedUnit =
    div [ class "flex-1 flex flex-col items-center" ] <|
        div [ class "flex items-center pb-4"]
            [ Flag.view player.color
            , h2 [] [ text player.name ]
            ]
            :: List.map
                (\unit ->
                    let
                        maxHp =
                            toFloat (Game.UnitType.initialValues unit.unitType |> .health)

                        hpLost =
                            maxHp - toFloat unit.health
                    in
                    div
                        [ class
                            ("flex flex-col items-center p-2 w-24"
                                ++ (if unit.id == focusedUnit.id then
                                        " bg-juralen-transparent"

                                    else
                                        " "
                                   )
                            )
                        ]
                        [ img [ class "w-8", src (Game.UnitType.icon unit.unitType), class "unit" ] []
                        , text (Game.UnitType.toString unit.unitType)
                        , div [ class "h-2 flex w-full m-2" ]
                            [ div [ class "bg-green-500", style "width" (String.fromFloat (100 * (toFloat unit.health / maxHp)) ++ "%") ]
                                []
                            , div [ class "bg-red-500", style "width" (String.fromFloat (100 * (hpLost / maxHp)) ++ "%") ] []
                            ]
                        ]
                )
                units
