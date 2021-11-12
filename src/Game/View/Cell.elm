module Game.View.Cell exposing (view)

import Game.Analysis
import Game.Cell exposing (Cell, Loc)
import Game.CellType
import Game.Core exposing (..)
import Game.Player exposing (NewPlayer, isHuman)
import Game.PlayerColor
import Game.Scenario as Scenario
import Game.Scoreboard as Scoreboard
import Game.Structure
import Game.TechTree as TechTree exposing (TechDescription, TechLevel(..))
import Game.Unit exposing (Unit, isSelected)
import Game.UnitType
import Game.Update exposing (Msg(..), update)
import Html exposing (Attribute, Html, br, button, div, img, span, table, td, text, tr)
import Html.Attributes exposing (class, disabled, src, style)
import Html.Events exposing (onClick, preventDefaultOn)
import Html.Lazy exposing (lazy)


view :
    Cell
    ->
        { isInRange : Bool
        , isSelected : Bool
        , cellColor : String
        , onCellClick : msg
        , units : List Unit
        , selectedUnits : List Int
        }
    -> Html msg
view cell { isInRange, isSelected, cellColor, onCellClick, units, selectedUnits } =
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
