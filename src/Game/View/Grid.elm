module Game.View.Grid exposing (..)

import Game.Cell exposing (Cell)
import Game.Core exposing (..)
import Game.Grid exposing (Grid)
import Game.Loc exposing (Loc)
import Game.Unit exposing (Unit)
import Game.Update exposing (Msg(..))
import Game.View.Cell as Cell
import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Html.Lazy exposing (lazy6)


view :
    Grid
    ->
        { isInRange : Cell -> Bool
        , selectedCell : Loc
        , getCellColor : Cell -> String
        , onCellClick : Cell -> msg
        , unitsInCell : Cell -> List Unit
        , selectedUnits : List Int
        , zoomIn : msg
        , zoomOut : msg
        }
    -> Html msg
view grid { isInRange, selectedCell, getCellColor, onCellClick, unitsInCell, selectedUnits, zoomIn, zoomOut } =
    div [ class "relative" ]
        [ div [ class "h-[350px] lg:h-[700px] flex overflow-scroll shadow-inner border-2 border-gray-100 shadow-inner" ]
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
        , button
            [ class "absolute bottom-[12px] right-[-12px] h-6 w-6 bg-gray-100 border-2 border-gray-100 rounded-full flex justify-center items-center hover:bg-gray-300 hover:border-gray-400 transition duration-200 shadow hover:shadow-md"
            , onClick zoomIn
            ]
            [ text "+"
            ]
        , button
            [ class "absolute bottom-[-12px] right-[12px] h-6 w-6 bg-gray-100 border-2 border-gray-100 rounded-full flex justify-center items-center hover:bg-gray-300 hover:border-gray-400 transition duration-200 shadow hover:shadow-md"
            , onClick zoomOut
            ]
            [ text "-"
            ]
        ]
