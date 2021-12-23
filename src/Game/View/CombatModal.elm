module Game.View.CombatModal exposing (view)

import Components.Modal as Modal
import Game.Combat as Combat
import Game.Unit exposing (Unit)
import Game.UnitType
import Html exposing (Html, div, h1, h2, img, text)
import Html.Attributes exposing (class, src, style)


view :
    { show : Bool
    , onClose : msg
    , model : Combat.Model
    }
    -> Html msg
view config =
    let
        attackingUnits =
            List.filter (\u -> u.controlledBy == config.model.attackingPlayer.id) config.model.units

        defendingUnits =
            List.filter (\u -> u.controlledBy == config.model.defendingPlayer.id) config.model.units
    in
    Modal.view
        { show = config.show
        , onClose = config.onClose
        , content =
            div [ class "text-white" ]
                [ h1 [] [ text "Combat" ]
                , div [ class "flex" ]
                    [ unitListDisplay attackingUnits
                    , unitListDisplay defendingUnits
                    ]
                , if List.length attackingUnits <= 0 then
                    h2 [ class "text-center" ]
                        [ text <| config.model.defendingPlayer.name ++ " wins!" ]

                  else if List.length defendingUnits <= 0 then
                    h2 [ class "text-center" ] [ text <| config.model.attackingPlayer.name ++ " wins!" ]

                  else
                    text ""
                ]
        }


unitListDisplay : List Unit -> Html msg
unitListDisplay units =
    div [ class "flex-1 flex flex-col items-center" ]
        (List.map
            (\unit ->
                let
                    maxHp =
                        toFloat (Game.UnitType.initialValues unit.unitType |> .health)

                    hpLost =
                        maxHp - toFloat unit.health
                in
                div [ class "flex flex-col items-center py-2" ]
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
