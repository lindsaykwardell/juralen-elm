module Game.Grid exposing (..)

import Game.Cell exposing (Cell, getDistance)
import Game.Loc exposing (Loc)
import Game.Structure


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
    List.foldl
        (\row distance ->
            List.foldl
                (\cell closest ->
                    case cell.controlledBy of
                        Nothing ->
                            closest

                        Just controller ->
                            let
                                distanceToCell =
                                    getDistance loc { x = cell.x, y = cell.y }
                            in
                            if controller /= playerId && distanceToCell < closest then
                                distanceToCell

                            else
                                closest
                )
                distance
                row
        )
        100
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
                                            + cell.farms
                                            + (case cell.structure of
                                                Nothing ->
                                                    0

                                                Just structure ->
                                                    case structure of
                                                        Game.Structure.Town ->
                                                            1

                                                        Game.Structure.Citadel ->
                                                            2

                                                        Game.Structure.None ->
                                                            0
                                              )

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
