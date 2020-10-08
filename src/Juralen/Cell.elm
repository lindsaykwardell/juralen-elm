module Juralen.Cell exposing (..)

import Juralen.CellType exposing (CellType)
import Juralen.Player exposing (Player)
import Juralen.Structure exposing (Structure)


type alias Loc =
    { x : Int
    , y : Int
    }


type alias Cell =
    { cellType : CellType
    , controlledBy : Maybe Int
    , defBonus : Int
    , structure : Maybe Structure
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
        , x = loc.x
        , y = loc.y
        }

    else if roll > 12 && roll <= 20 then
        { cellType = Juralen.CellType.Mountain
        , controlledBy = Nothing
        , defBonus = 0
        , structure = Nothing
        , x = loc.x
        , y = loc.y
        }

    else if roll > 20 && roll <= 40 then
        { cellType = Juralen.CellType.Forest
        , controlledBy = Nothing
        , defBonus = 1
        , structure = Nothing
        , x = loc.x
        , y = loc.y
        }

    else
        { cellType = Juralen.CellType.Plains
        , controlledBy = Nothing
        , defBonus = 0
        , structure = Nothing
        , x = loc.x
        , y = loc.y
        }


empty : Cell
empty =
    { cellType = Juralen.CellType.Plains
    , controlledBy = Nothing
    , defBonus = -1
    , structure = Nothing
    , x = -1
    , y = -1
    }


find : List (List Cell) -> Loc -> Maybe Cell
find grid loc =
    Maybe.andThen (\row -> List.head (List.filter (\innerCell -> innerCell.x == loc.x && innerCell.y == loc.y) row)) (List.head (List.filter (\row -> List.length (List.filter (\innerCell -> innerCell.x == loc.x && innerCell.y == loc.y) row) > 0) grid))


atLoc : List Cell -> Loc -> Cell
atLoc cellList loc =
    let
        nextCell =
            List.head cellList

        remainingCells =
            case List.tail cellList of
                Nothing ->
                    []

                Just nextCellList ->
                    nextCellList
    in
    case nextCell of
        Nothing ->
            empty

        Just cell ->
            if cell.x == loc.x && cell.y == loc.y then
                cell

            else
                atLoc remainingCells loc


validStartingCell : List (List Cell) -> Loc -> Maybe Cell
validStartingCell grid loc =
    Maybe.andThen (\row -> List.head (List.filter (\innerCell -> innerCell.x == loc.x && innerCell.y == loc.y) row)) (List.head (List.filter (\row -> List.length (List.filter (\innerCell -> innerCell.x == loc.x && innerCell.y == loc.y && hasStructure innerCell == False) row) > 0) grid))


hasStructure : Cell -> Bool
hasStructure cell =
    case cell.structure of
        Nothing ->
            False

        _ ->
            True


buildStructure : Cell -> Structure -> Cell
buildStructure cell structure =
    { cell | structure = Just structure, cellType = Juralen.CellType.Plains, defBonus = 7 }


updateControl : Cell -> Int -> Cell
updateControl cell playerId =
    if cell.cellType == Juralen.CellType.Plains then 
        { cell | controlledBy = Just playerId }
    else cell


getColorClass : Cell -> List Player -> String
getColorClass cell players =
    case cell.controlledBy of
        Nothing ->
            Juralen.CellType.getColorClass cell.cellType

        _ ->
            "bg-" ++ Juralen.Player.getColorClass players cell.controlledBy


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
