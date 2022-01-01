module Game.PlayerColor exposing (..)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode
import Json.Encode as Encode


type PlayerColor
    = Red
    | Blue
    | Green
    | Orange
    | Teal
    | Purple
    | Yellow
    | Gray
    | None


decoder : Decoder PlayerColor
decoder =
    Decode.string
        |> Decode.andThen
            (\color ->
                case color of
                    "red" ->
                        Decode.succeed Red

                    "blue" ->
                        Decode.succeed Blue

                    "green" ->
                        Decode.succeed Green

                    "orange" ->
                        Decode.succeed Orange

                    "teal" ->
                        Decode.succeed Teal

                    "purple" ->
                        Decode.succeed Purple

                    "yellow" ->
                        Decode.succeed Yellow

                    "gray" ->
                        Decode.succeed Gray

                    "none" ->
                        Decode.succeed None

                    _ ->
                        Decode.fail "Invalid color"
            )


encoder : PlayerColor -> Encode.Value
encoder color =
    case color of
        Red ->
            Encode.string "red"

        Blue ->
            Encode.string "blue"

        Green ->
            Encode.string "green"

        Orange ->
            Encode.string "orange"

        Teal ->
            Encode.string "teal"

        Purple ->
            Encode.string "purple"

        Yellow ->
            Encode.string "yellow"

        Gray ->
            Encode.string "gray"

        None ->
            Encode.string "none"


toList : List PlayerColor
toList =
    [ Red, Blue, Green, Orange, Teal, Purple, Yellow, Gray ]


toString : PlayerColor -> String
toString playerColor =
    case playerColor of
        Red ->
            "red"

        Blue ->
            "blue"

        Green ->
            "green"

        Orange ->
            "orange"

        Teal ->
            "teal"

        Yellow ->
            "yellow"

        Purple ->
            "purple"

        Gray ->
            "gray"

        None ->
            ""


fromString : String -> PlayerColor
fromString color =
    case color of
        "red" ->
            Red

        "blue" ->
            Blue

        "green" ->
            Green

        "orange" ->
            Orange

        "teal" ->
            Teal

        "yellow" ->
            Yellow

        "purple" ->
            Purple

        "gray" ->
            Gray

        _ ->
            None


toClass : PlayerColor -> String
toClass playerColor =
    case playerColor of
        Red ->
            "bg-player-red"

        Blue ->
            "bg-player-blue"

        Green ->
            "bg-player-green"

        Orange ->
            "bg-player-orange"

        Teal ->
            "bg-player-teal"

        Yellow ->
            "bg-player-yellow"

        Purple ->
            "bg-player-purple"

        Gray ->
            "bg-player-gray"

        None ->
            ""


toTextClass : PlayerColor -> String
toTextClass playerColor =
    case playerColor of
        Red ->
            "text-player-red"

        Blue ->
            "text-player-blue"

        Green ->
            "text-player-green"

        Orange ->
            "text-player-orange"

        Teal ->
            "text-player-teal"

        Yellow ->
            "text-player-yellow"

        Purple ->
            "text-player-purple"

        Gray ->
            "text-player-gray"

        None ->
            ""


isDark : PlayerColor -> Bool
isDark playerColor =
    case playerColor of
        Red ->
            True

        Blue ->
            True

        Green ->
            False

        Orange ->
            False

        Teal ->
            False

        Yellow ->
            False

        Purple ->
            True

        Gray ->
            True

        None ->
            False
