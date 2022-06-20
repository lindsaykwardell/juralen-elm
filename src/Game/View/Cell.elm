module Game.View.Cell exposing (view)

import Game.Cell exposing (Cell)
import Game.Structure
import Game.Unit exposing (Unit)
import Game.UnitType
import Html exposing (Html, div, img, span, td, text)
import Html.Attributes exposing (class, src, style)
import Html.Events exposing (onClick)


view :
    Cell
    -> Bool
    -> Bool
    -> String
    -> msg
    -> List Unit
    -> List Int
    -> Html msg
view cell isInRange isSelected cellColor onCellClick units selectedUnits =
    td []
        [ div
            [ class
                ("cell "
                    ++ (if isInRange then
                            "in-range "

                        else
                            ""
                       )
                    ++ Game.Structure.getCellClass cell.structure
                    ++ " "
                    ++ cellColor
                )
            , style "border"
                (if isSelected then
                    "2px solid yellow"

                 else
                    ""
                )
            , onClick onCellClick
            ]
            [ div []
                (List.map
                    (\unit ->
                        span [ class "unit" ]
                            [ if Game.Unit.isSelected selectedUnits unit.id then
                                span []
                                    [ text "[ "
                                    , img [ src (Game.UnitType.icon unit.unitType), class "unit" ] []
                                    , text " ]"
                                    ]

                              else
                                span [] [ img [ src (Game.UnitType.icon unit.unitType), class "unit" ] [] ]
                            ]
                    )
                    units
                )
            ]
        ]
