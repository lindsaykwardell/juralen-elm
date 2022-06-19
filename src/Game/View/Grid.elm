module Game.View.Grid exposing (..)

import Game.Cell exposing (Cell)
import Game.Core exposing (..)
import Game.Grid exposing (Grid)
import Game.Loc exposing (Loc)
import Game.Unit exposing (Unit)
import Game.View.Cell as Cell
import Html exposing (Html, div, table, tr)
import Html.Attributes exposing (class)
import Html.Lazy exposing (lazy, lazy6)


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
    div [ class "h-[350px] lg:h-[700px] flex overflow-scroll shadow-inner border-2 border-gray-100 shadow-inner" ]
        [ table [ class "m-auto py-[350px]" ]
            (List.map
                (\row ->
                    tr []
                        (List.map
                            (\cell ->
                                lazy6
                                    (Cell.view cell)
                                    (isInRange cell)
                                    (cell.loc == selectedCell)
                                    (getCellColor cell)
                                    (onCellClick cell)
                                    (unitsInCell cell)
                                    selectedUnits
                            )
                            row
                        )
                )
                (Game.Grid.toMatrix grid)
            )
        ]
