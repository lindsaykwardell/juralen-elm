module Game.CellType exposing (CellType(..), decoder, encoder, getColorClass, isPassable, toString)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


type CellType
    = Plains
    | Forest
    | Mountain


decoder : Decoder CellType
decoder =
    Decode.string
        |> Decode.andThen
            (\cellType ->
                case cellType of
                    "plains" ->
                        Decode.succeed Plains

                    "forest" ->
                        Decode.succeed Forest

                    "mountain" ->
                        Decode.succeed Mountain

                    _ ->
                        Decode.fail "Invalid cell type"
            )


encoder : CellType -> Encode.Value
encoder cellType =
    case cellType of
        Plains ->
            Encode.string "plains"

        Forest ->
            Encode.string "forest"

        Mountain ->
            Encode.string "mountain"


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
    cellType /= Mountain
