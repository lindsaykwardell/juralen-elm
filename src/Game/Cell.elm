module Game.Cell exposing (..)

import Game.CellType exposing (CellType)
import Game.Loc as Loc exposing (Loc)
import Game.Player exposing (Player)
import Game.PlayerColor
import Game.Structure as Structure exposing (Structure)
import List.Extra


type alias Cell =
    { cellType : CellType
    , controlledBy : Maybe Int
    , defBonus : Int
    , structure : Structure
    , farms : Int
    , towers : Int
    , loc : Loc
    }


generate : Loc -> Int -> Cell
generate loc roll =
    if roll <= 12 then
        { cellType = Game.CellType.Plains
        , controlledBy = Nothing
        , defBonus = 3
        , structure = Structure.Town
        , farms = 0
        , towers = 0
        , loc = loc
        }

    else if roll > 12 && roll <= 20 then
        { cellType = Game.CellType.Mountain
        , controlledBy = Nothing
        , defBonus = 0
        , structure = Structure.None
        , farms = 0
        , towers = 0
        , loc = loc
        }

    else if roll > 20 && roll <= 40 then
        { cellType = Game.CellType.Forest
        , controlledBy = Nothing
        , defBonus = 1
        , structure = Structure.None
        , farms = 0
        , towers = 0
        , loc = loc
        }

    else
        { cellType = Game.CellType.Plains
        , controlledBy = Nothing
        , defBonus = 0
        , structure = Structure.None
        , farms = 0
        , towers = 0
        , loc = loc
        }


empty : Cell
empty =
    { cellType = Game.CellType.Plains
    , controlledBy = Nothing
    , defBonus = -1
    , structure = Structure.None
    , farms = 0
    , towers = 0
    , loc = Loc.at -1 -1
    }


find : List (List Cell) -> Loc -> Maybe Cell
find grid loc =
    Maybe.andThen
        (\row ->
            List.head
                (List.filter
                    (\innerCell -> innerCell.loc == loc)
                    row
                )
        )
        (List.head
            (List.filter
                (\row ->
                    List.length
                        (List.filter
                            (\innerCell -> innerCell.loc == loc)
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
                (List.filter (\innerCell -> innerCell.loc == loc) row)
        )
        (List.head
            (List.filter
                (\row ->
                    List.length
                        (List.filter (\innerCell -> innerCell.loc == loc && hasStructure innerCell == False) row)
                        > 0
                )
                grid
            )
        )


hasStructure : Cell -> Bool
hasStructure cell =
    cell.structure /= Structure.None


buildStructure : Cell -> Structure -> Cell
buildStructure cell structure =
    { cell | structure = structure, cellType = Game.CellType.Plains, defBonus = 5 }


updateControl : Cell -> Int -> Cell
updateControl cell playerId =
    if cell.cellType == Game.CellType.Plains then
        { cell | controlledBy = Just playerId }

    else
        cell


getColorClass : Cell -> List Player -> String
getColorClass cell players =
    case cell.controlledBy of
        Nothing ->
            Game.CellType.getColorClass cell.cellType

        Just playerId ->
            Game.Player.get players playerId
                |> .color
                |> Game.PlayerColor.toClass


getBorderCells : List (List Cell) -> Loc -> List (Maybe Cell)
getBorderCells grid loc =
    let
        north =
            Loc.diff loc 0 -1

        south =
            Loc.diff loc 0 1

        east =
            Loc.diff loc 1 0

        west =
            Loc.diff loc -1 0
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
                    if cell.cellType == Game.CellType.Mountain then
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
                                                    cell.loc

                                                groupLoc =
                                                    cellInGroup.loc

                                                north =
                                                    Loc.diff loc 0 -1

                                                south =
                                                    Loc.diff loc 0 1

                                                east =
                                                    Loc.diff loc 1 0

                                                west =
                                                    Loc.diff loc -1 0
                                            in
                                            cell.controlledBy == cellInGroup.controlledBy && ((loc == groupLoc) || (north == groupLoc) || (south == groupLoc) || (east == groupLoc) || (west == groupLoc))
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


getGroup : List (List Cell) -> Loc -> Maybe (List Cell)
getGroup groups loc =
    List.Extra.find (\g -> List.Extra.find (\cell -> cell.loc == loc) g /= Nothing) groups


getGroupBorderingPlayers : List (List Cell) -> Loc -> List (List Cell) -> List (Maybe Int)
getGroupBorderingPlayers grid loc groups =
    case getGroup groups loc of
        Nothing ->
            []

        Just cells ->
            List.foldl
                (\cell players ->
                    getBorderingPlayers grid cell.loc ++ players
                )
                []
                cells
