module Juralen.Node exposing (..)

import Juralen.Cell exposing (Cell, Loc, getBorderCells)


type alias Node =
    { controlledBy : Maybe Int
    , locList : List Loc
    }


getNodeList : List (List Cell) -> List Node
getNodeList grid =
    List.foldl
        (\row nodes ->
            List.foldl
                (\cell rowNodes ->
                    let
                        borderingCells =
                            getBorderCells grid { x = cell.x, y = cell.y }

                        bordersMatchingNode : Bool
                        bordersMatchingNode =
                            List.any
                                (\maybeCell ->
                                    case maybeCell of
                                        Nothing ->
                                            False

                                        Just borderCell ->
                                            borderCell.controlledBy == cell.controlledBy
                                )
                                borderingCells
                    in
                    rowNodes
                )
                nodes
                row
        )
        []
        grid
