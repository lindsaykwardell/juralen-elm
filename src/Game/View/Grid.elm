module Game.View.Grid exposing (..)

import Game.Cell exposing (Cell)
import Game.Loc exposing (Loc)
import Game.Core exposing (..)
import Game.Grid exposing (Grid)
import Game.Unit exposing (Unit)
import Game.View.Cell as Cell
import Html exposing (Html, div, table, tr)
import Html.Attributes exposing (class)
import Html.Lazy exposing (lazy)


view :
    Grid
    ->
        { isInRange : Cell -> Bool
        , selectedCell : Loc
        , getCellColor : Cell -> String
        , onCellClick : Cell -> msg
        , unitsInCell : Cell -> List Unit
        , selectedUnits : List Int
        }
    -> Html msg
view grid { isInRange, selectedCell, getCellColor, onCellClick, unitsInCell, selectedUnits } =
    div [ class "max-h-[350px] lg:max-h-[800px] overflow-scroll shadow-inner rounded-lg border-2 border-gray-100 shadow-inner" ]
        [ table [ class "m-auto" ]
            (List.map
                (\row ->
                    tr []
                        (List.map
                            (\cell ->
                                lazy
                                    (Cell.view cell)
                                    { isInRange = isInRange cell
                                    , isSelected = cell.loc == selectedCell
                                    , cellColor = getCellColor cell
                                    , onCellClick = onCellClick cell
                                    , units = unitsInCell cell
                                    , selectedUnits = selectedUnits
                                    }
                            )
                            row
                        )
                )
                grid
            )
        ]
