module Juralen.Grid exposing (..)

import Juralen.Cell exposing (Cell, Loc, getDistance)
import Juralen.Structure


type alias Grid =
    List (List Cell)



toList : Grid -> List Cell
toList grid =
    addNextRow grid []

addNextRow : Grid -> List Cell -> List Cell
addNextRow grid cellList =
    let
        firstRow : List Cell
        firstRow =
            case List.head grid of
                Nothing ->
                    []

                Just row ->
                    row

        remainingRows : List (List Cell)
        remainingRows =
            case List.tail grid of
                Nothing ->
                    []

                Just rows ->
                    rows

        updatedList : List Cell
        updatedList =
            cellList ++ firstRow
    in
    if List.length firstRow <= 0 then
        updatedList

    else
        addNextRow remainingRows updatedList


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

distanceToEnemy : Grid -> Loc -> Int -> Int
distanceToEnemy grid loc playerId =
    List.foldl (\cell closest -> 
        case cell.controlledBy of
            Nothing -> 
                closest
            
            Just controller ->
                let
                    distance = getDistance loc { x = cell.x, y = cell.y }
                in
                    if controller /= playerId && distance < closest then distance else closest
    ) 100 (toList grid)
    

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
                                        1 + cell.farms
                                        + case cell.structure of
                                            Nothing ->
                                                0

                                            Just structure ->
                                                case structure of
                                                    Juralen.Structure.Town ->
                                                        1

                                                    Juralen.Structure.Citadel ->
                                                        2

                                                    Juralen.Structure.None ->
                                                        0

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
                                        1 + cell.towers

                                    else
                                        0
                              )
                    )
                    0
                    row
        )
        0
        grid