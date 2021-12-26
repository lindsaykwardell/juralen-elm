module Game.View.CombatModal exposing (view)

import Components.Modal as Modal
import Game.CellType
import Game.Combat as Combat
import Game.Loc as Loc
import Game.Unit exposing (Unit)
import Game.UnitType
import Game.Update exposing (Msg(..))
import Html exposing (Html, button, div, h1, h2, img, text)
import Html.Attributes exposing (class, src, style)
import Html.Events exposing (onClick)


view :
    { show : Bool
    , model : Combat.Model
    }
    -> Html Msg
view config =
    let
        attackingUnits =
            List.filter (\u -> u.controlledBy == config.model.attackingPlayer.id) config.model.units

        defendingUnits =
            List.filter (\u -> u.controlledBy == config.model.defendingPlayer.id) config.model.units

        combatEnded =
            List.length attackingUnits <= 0 || List.length defendingUnits <= 0
    in
    Modal.view
        { show = config.show
        , onClose =
            if combatEnded then
                GotCombatMsg Combat.ExitCombat

            else
                NoOp
        , content =
            div [ class ("text-white p-2 " ++ Game.CellType.getColorClass config.model.cell.cellType) ]
                [ h1 []
                    [ text "( "
                    , text (String.fromInt (Loc.getX config.model.cell.loc))
                    , text ", "
                    , text (String.fromInt (Loc.getY config.model.cell.loc))
                    , text " )"
                    ]
                , div [ class "flex" ]
                    [ unitListDisplay attackingUnits config.model.attacker
                    , unitListDisplay defendingUnits config.model.defender
                    ]
                , if List.length attackingUnits <= 0 then
                    h2 [ class "text-center" ]
                        [ text <| config.model.defendingPlayer.name ++ " wins!" ]

                  else if List.length defendingUnits <= 0 then
                    h2 [ class "text-center" ] [ text <| config.model.attackingPlayer.name ++ " wins!" ]

                  else
                    text ""
                , if combatEnded then
                    div [ class "text-center" ]
                        [ button [ class "px-6 py-2 bg-gray-700 rounded hover:bg-gray-800 transition duration-50", onClick (GotCombatMsg Combat.ExitCombat) ] [ text "Close" ] ]

                  else
                    text ""
                ]
        }


unitListDisplay : List Unit -> Unit -> Html msg
unitListDisplay units focusedUnit =
    div [ class "flex-1 flex flex-col items-center" ]
        (List.map
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
                                    " bg-juralen-transparent rounded-lg"

                                else
                                    " "
                               )
                        )
                    ]
                    [ img [ class "w-8", src (Game.UnitType.icon unit.unitType), class "unit" ] []
                    , text (Game.UnitType.toString unit.unitType { showCost = False })
                    , div [ class "h-2 flex w-full m-2" ]
                        [ div [ class "bg-green-500", style "width" (String.fromFloat (100 * (toFloat unit.health / maxHp)) ++ "%") ]
                            []
                        , div [ class "bg-red-500", style "width" (String.fromFloat (100 * (hpLost / maxHp)) ++ "%") ] []
                        ]
                    ]
            )
            units
        )