module Juralen.CellType exposing (..)


type CellType
    = Plains
    | Forest
    | Mountain


toString : CellType -> String
toString cellType =
    case cellType of
        Plains ->
            "Plains"

        Forest ->
            "Forest"

        Mountain ->
            "Mountain"


getColorClass : CellType -> String
getColorClass cellType =
    case cellType of
        Plains ->
            "terrain-plains"

        Forest ->
            "terrain-forest"

        Mountain ->
            "terrain-mountain"

isPassable : CellType -> Bool
isPassable cellType =
    case cellType of
        Mountain ->
            False

        _ ->
            True