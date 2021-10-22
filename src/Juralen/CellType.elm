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
            "bg-terrain-plains"

        Forest ->
            "bg-terrain-forest"

        Mountain ->
            "bg-terrain-mountain"


isPassable : CellType -> Bool
isPassable cellType =
    cellType == Plains
