module Game.AnalyzerMode exposing (AnalyzerMode(..), decoder, encoder)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


type AnalyzerMode
    = Default
    | Aggressive
    | Defensive
    | Passive
    | Expansionist


decoder : Decoder AnalyzerMode
decoder =
    Decode.string
        |> Decode.andThen
            (\mode ->
                case mode of
                    "default" ->
                        Decode.succeed Default

                    "aggressive" ->
                        Decode.succeed Aggressive

                    "defensive" ->
                        Decode.succeed Defensive

                    "passive" ->
                        Decode.succeed Passive

                    "expansionist" ->
                        Decode.succeed Expansionist

                    _ ->
                        Decode.fail "Invalid analyzer mode"
            )


encoder : AnalyzerMode -> Encode.Value
encoder mode =
    case mode of
        Default ->
            Encode.string "default"

        Aggressive ->
            Encode.string "aggressive"

        Defensive ->
            Encode.string "defensive"

        Passive ->
            Encode.string "passive"

        Expansionist ->
            Encode.string "expansionist"
