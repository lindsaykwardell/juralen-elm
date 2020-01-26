module Juralen.Grid exposing (..)

import Juralen.Cell exposing (Cell)


type alias Grid =
    List (List Cell)


replaceCell : Grid -> Cell -> Grid
replaceCell grid newCell =
    List.map
        (\row ->
            List.map
                (\cell ->
                    if cell.x == newCell.x && cell.y == newCell.y then
                        newCell

                    else
                        cell
                )
                row
        )
        grid


farmCountControlledBy : Grid -> Int -> Int
farmCountControlledBy grid playerId =
    List.foldl
        (\row total ->
            total
                + List.foldl
                    (\cell rowTotal ->
                        rowTotal
                            + (case cell.controlledBy of
                                Nothing ->
                                    0

                                Just controlledBy ->
                                    if controlledBy == playerId then
                                        1

                                    else
                                        0
                              )
                    )
                    0
                    row
        )
        0
        grid


townCountControlledBy : Grid -> Int -> Int
townCountControlledBy grid playerId =
    List.foldl
        (\row total ->
            total
                + List.foldl
                    (\cell rowTotal ->
                        rowTotal
                            + (case cell.controlledBy of
                                Nothing ->
                                    0

                                Just controlledBy ->
                                    if cell.structure /= Nothing && controlledBy == playerId then
                                        1

                                    else
                                        0
                              )
                    )
                    0
                    row
        )
        0
        grid
