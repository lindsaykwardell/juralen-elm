module Juralen.Cell exposing (..)

import Juralen.CellType exposing (CellType)
import Juralen.Player exposing (Player)
import Juralen.Structure exposing (Structure)
import List.Extra


type alias Loc =
    { x : Int
    , y : Int
    }


type alias Cell =
    { cellType : CellType
    , controlledBy : Maybe Int
    , defBonus : Int
    , structure : Maybe Structure
    , farms : Int
    , towers : Int
    , x : Int
    , y : Int
    }


generate : Loc -> Int -> Cell
generate loc roll =
    if roll <= 12 then
        { cellType = Juralen.CellType.Plains
        , controlledBy = Nothing
        , defBonus = 3
        , structure = Just Juralen.Structure.Town
        , farms = 0
        , towers = 0
        , x = loc.x
        , y = loc.y
        }

    else if roll > 12 && roll <= 20 then
        { cellType = Juralen.CellType.Mountain
        , controlledBy = Nothing
        , defBonus = 0
        , structure = Nothing
        , farms = 0
        , towers = 0
        , x = loc.x
        , y = loc.y
        }

    else if roll > 20 && roll <= 40 then
        { cellType = Juralen.CellType.Forest
        , controlledBy = Nothing
        , defBonus = 1
        , structure = Nothing
        , farms = 0
        , towers = 0
        , x = loc.x
        , y = loc.y
        }

    else
        { cellType = Juralen.CellType.Plains
        , controlledBy = Nothing
        , defBonus = 0
        , structure = Nothing
        , farms = 0
        , towers = 0
        , x = loc.x
        , y = loc.y
        }


empty : Cell
empty =
    { cellType = Juralen.CellType.Plains
    , controlledBy = Nothing
    , defBonus = -1
    , structure = Nothing
    , farms = 0
    , towers = 0
    , x = -1
    , y = -1
    }


find : List (List Cell) -> Loc -> Maybe Cell
find grid loc =
    Maybe.andThen
        (\row ->
            List.head
                (List.filter
                    (\innerCell -> innerCell.x == loc.x && innerCell.y == loc.y)
                    row
                )
        )
        (List.head
            (List.filter
                (\row ->
                    List.length
                        (List.filter
                            (\innerCell -> innerCell.x == loc.x && innerCell.y == loc.y)
                            row
                        )
                        > 0
                )
                grid
            )
        )


atLoc : List (List Cell) -> Loc -> Cell
atLoc cellList loc =
    Maybe.withDefault empty (find cellList loc)


ofType : CellType -> List (List Cell) -> List Cell
ofType cellType grid =
    List.filter
        (\cell -> cell.cellType == cellType)
        (List.concat grid)


validStartingCell : List (List Cell) -> Loc -> Maybe Cell
validStartingCell grid loc =
    Maybe.andThen
        (\row ->
            List.head
                (List.filter (\innerCell -> innerCell.x == loc.x && innerCell.y == loc.y) row)
        )
        (List.head
            (List.filter
                (\row ->
                    List.length
                        (List.filter (\innerCell -> innerCell.x == loc.x && innerCell.y == loc.y && hasStructure innerCell == False) row)
                        > 0
                )
                grid
            )
        )


hasStructure : Cell -> Bool
hasStructure cell =
    cell.structure /= Nothing


buildStructure : Cell -> Structure -> Cell
buildStructure cell structure =
    { cell | structure = Just structure, cellType = Juralen.CellType.Plains, defBonus = 5 }


updateControl : Cell -> Int -> Cell
updateControl cell playerId =
    if cell.cellType == Juralen.CellType.Plains then
        { cell | controlledBy = Just playerId }

    else
        cell


getColorClass : Cell -> List Player -> String
getColorClass cell players =
    case cell.controlledBy of
        Nothing ->
            Juralen.CellType.getColorClass cell.cellType

        _ ->
            Juralen.Player.getColorClass players cell.controlledBy


getDistance : Loc -> Loc -> Int
getDistance from to =
    let
        x =
            if (from.x - to.x) < 0 then
                (from.x - to.x) * -1

            else
                from.x - to.x

        y =
            if (from.y - to.y) < 0 then
                (from.y - to.y) * -1

            else
                from.y - to.y
    in
    x + y


getBorderCells : List (List Cell) -> Loc -> List (Maybe Cell)
getBorderCells grid loc =
    let
        north =
            { loc | y = loc.y - 1 }

        south =
            { loc | y = loc.y + 1 }

        east =
            { loc | x = loc.x + 1 }

        west =
            { loc | x = loc.x - 1 }
    in
    [ find grid north
    , find grid south
    , find grid east
    , find grid west
    ]


getBorderingPlayers : List (List Cell) -> Loc -> List (Maybe Int)
getBorderingPlayers grid loc =
    let
        borderingCells =
            getBorderCells grid loc
    in
    getBorderingPlayer borderingCells []


getBorderingPlayer : List (Maybe Cell) -> List (Maybe Int) -> List (Maybe Int)
getBorderingPlayer cells players =
    case cells of
        [] ->
            players

        maybeCell :: remainingCells ->
            case maybeCell of
                Nothing ->
                    getBorderingPlayer remainingCells players

                Just cell ->
                    if cell.cellType == Juralen.CellType.Mountain then
                        getBorderingPlayer remainingCells players

                    else
                        getBorderingPlayer remainingCells (players ++ [ cell.controlledBy ])


groupNeighbors : List Cell -> List (List Cell)
groupNeighbors cells =
    List.foldl
        (\cell groups ->
            case groups of
                [] ->
                    [ [ cell ] ]

                _ ->
                    let
                        groupIndex : Maybe Int
                        groupIndex =
                            List.Extra.findIndex
                                (\group ->
                                    List.Extra.find
                                        (\cellInGroup ->
                                            let
                                                loc =
                                                    { x = cell.x
                                                    , y = cell.y
                                                    }

                                                groupLoc =
                                                    { x = cellInGroup.x
                                                    , y = cellInGroup.y
                                                    }

                                                north =
                                                    { loc | y = loc.y - 1 }

                                                south =
                                                    { loc | y = loc.y + 1 }

                                                east =
                                                    { loc | x = loc.x + 1 }

                                                west =
                                                    { loc | x = loc.x - 1 }
                                            in
                                            (loc == groupLoc) || (north == groupLoc) || (south == groupLoc) || (east == groupLoc) || (west == groupLoc)
                                        )
                                        group
                                        /= Nothing
                                )
                                groups
                    in
                    case groupIndex of
                        Nothing ->
                            groups ++ [ [ cell ] ]

                        Just id ->
                            let
                                group =
                                    List.Extra.getAt id groups
                            in
                            List.Extra.setAt id (Maybe.withDefault [] group ++ [ cell ]) groups
        )
        []
        cells


getGroupBorderingPlayers : List (List Cell) -> List (List Cell) -> Loc -> List (Maybe Int)
getGroupBorderingPlayers grid groups loc =
    let
        group : Maybe (List Cell)
        group =
            List.Extra.find (\g -> List.Extra.find (\cell -> cell.x == loc.x && cell.y == loc.y) g /= Nothing) groups
    in
    case group of
        Nothing ->
            []

        Just cells ->
            List.foldl
                (\cell players ->
                    getBorderingPlayers grid { x = cell.x, y = cell.y } ++ players
                )
                []
                cells
