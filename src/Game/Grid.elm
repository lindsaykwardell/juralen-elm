module Game.Grid exposing (..)

import Dict
import Game.Cell exposing (Cell)
import Game.CellType
import Game.Loc exposing (Loc, getDistance)
import Game.Structure as Structure
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode
import Json.Encode as Encode
import Sort
import Sort.Dict


type alias Grid =
    Sort.Dict.Dict Loc Cell


sorter : Sort.Sorter Loc
sorter =
    Sort.custom
        (\a b ->
            let
                ( ax, ay ) =
                    Game.Loc.coords a

                ( bx, by ) =
                    Game.Loc.coords b
            in
            if ax == bx then
                if ay == by then
                    EQ

                else if ay < by then
                    LT

                else
                    GT

            else if ax < bx then
                LT

            else
                GT
        )


decoder : Decoder Grid
decoder =
    Decode.list (Decode.list Game.Cell.decoder)
        |> Decode.andThen
            (fromMatrix
                >> Decode.succeed
            )


encoder : Grid -> Encode.Value
encoder grid =
    toMatrix grid
        |> Encode.list (Encode.list Game.Cell.encoder)


toMatrix : Grid -> List (List Cell)
toMatrix grid =
    Sort.Dict.toList grid
        |> List.reverse
        |> List.foldl
            (\( loc, cell ) rows ->
                let
                    cellY =
                        Game.Loc.coords loc |> Tuple.second
                in
                case Dict.get cellY rows of
                    Nothing ->
                        Dict.insert cellY [ cell ] rows

                    Just row ->
                        Dict.insert cellY (cell :: row) rows
            )
            Dict.empty
        |> Dict.toList
        |> List.map Tuple.second


fromMatrix : List (List Cell) -> Grid
fromMatrix =
    List.concat
        >> List.map (\cell -> ( cell.loc, cell ))
        >> Sort.Dict.fromList sorter


replaceCell : Grid -> Cell -> Grid
replaceCell grid newCell =
    Sort.Dict.insert newCell.loc newCell grid


distanceToEnemy : Grid -> Loc -> Int -> Int
distanceToEnemy grid loc playerId =
    Sort.Dict.toList grid
        |> List.foldl
            (\( _, cell ) closest ->
                case cell.controlledBy of
                    Nothing ->
                        closest

                    Just controller ->
                        let
                            distanceToCell =
                                getDistance loc cell.loc
                        in
                        if controller /= playerId && distanceToCell < closest then
                            distanceToCell

                        else
                            closest
            )
            100


farmCountControlledBy : Grid -> Int -> Int
farmCountControlledBy grid playerId =
    Sort.Dict.toList grid
        |> List.foldl
            (\( _, cell ) rowTotal ->
                rowTotal
                    + (case cell.controlledBy of
                        Nothing ->
                            0

                        Just controlledBy ->
                            if controlledBy == playerId then
                                1
                                    + cell.farms
                                    + (case cell.structure of
                                        Structure.Town ->
                                            1

                                        Structure.Citadel ->
                                            2

                                        Structure.None ->
                                            0
                                      )

                            else
                                0
                      )
            )
            0


townCountControlledBy : Grid -> Int -> Int
townCountControlledBy grid playerId =
    Sort.Dict.toList grid
        |> List.foldl
            (\( _, cell ) rowTotal ->
                rowTotal
                    + (case cell.controlledBy of
                        Nothing ->
                            0

                        Just controlledBy ->
                            if cell.structure /= Structure.None && controlledBy == playerId then
                                1 + cell.towers

                            else
                                0
                      )
            )
            0


validStartingCell : Grid -> Loc -> Maybe Cell
validStartingCell grid loc =
    Sort.Dict.get loc grid
        |> Maybe.andThen
            (\cell ->
                if Game.Cell.hasStructure cell then
                    Nothing

                else
                    Just cell
            )


getBorderCells : Grid -> Loc -> List (Maybe Cell)
getBorderCells grid loc =
    let
        north =
            Game.Loc.diff loc 0 -1

        south =
            Game.Loc.diff loc 0 1

        east =
            Game.Loc.diff loc 1 0

        west =
            Game.Loc.diff loc -1 0
    in
    [ Sort.Dict.get north grid
    , Sort.Dict.get south grid
    , Sort.Dict.get east grid
    , Sort.Dict.get west grid
    ]


getBorderingPlayers : Grid -> Loc -> List (Maybe Int)
getBorderingPlayers grid loc =
    let
        borderingCells =
            getBorderCells grid loc
    in
    Game.Cell.getBorderingPlayer borderingCells []


getGroupBorderingPlayers : Grid -> Loc -> List (List Cell) -> List (Maybe Int)
getGroupBorderingPlayers grid loc groups =
    case Game.Cell.getGroup groups loc of
        Nothing ->
            []

        Just cells ->
            List.foldl
                (\cell players ->
                    getBorderingPlayers grid cell.loc ++ players
                )
                []
                cells


ofType : Game.CellType.CellType -> Grid -> List Cell
ofType cellType =
    Sort.Dict.toList
        >> List.map Tuple.second
        >> List.filter
            (\cell -> cell.cellType == cellType)
